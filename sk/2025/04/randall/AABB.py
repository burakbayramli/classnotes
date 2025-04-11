# Converted by Gemini 2.5 
import math
from abc import ABC, abstractmethod
from typing import List, Dict, Optional, Any # Using Any for object temporarily

# Define a constant for null node index (-1 is common in Python for invalid index)
AABB_NULL_NODE = -1

# --- IAABB Interface ---
class IAABB(ABC):
    """Abstract base class for objects that can provide an AABB."""
    @abstractmethod
    def get_aabb(self) -> 'AABB':
        """Return the Axis-Aligned Bounding Box for the object."""
        pass

    # Make IAABB hashable for use as dictionary keys
    # Default object hash should work if subclasses don't override __eq__
    # If subclasses override __eq__, they MUST also override __hash__
    def __hash__(self):
        return super().__hash__()

    def __eq__(self, other):
        return super().__eq__(other)

# --- AABB Class ---
class AABB:
    """Represents an Axis-Aligned Bounding Box."""
    def __init__(self,
                 min_x: float = 0.0, min_y: float = 0.0, min_z: float = 0.0,
                 max_x: float = 0.0, max_y: float = 0.0, max_z: float = 0.0):
        # Ensure min <= max after potential initialization mix-up
        self.min_x = min(min_x, max_x)
        self.min_y = min(min_y, max_y)
        self.min_z = min(min_z, max_z)
        self.max_x = max(min_x, max_x)
        self.max_y = max(min_y, max_y)
        self.max_z = max(min_z, max_z)
        # Calculate surface area upon initialization
        self.surface_area = self._calculate_surface_area()

    def get_width(self) -> float:
        return self.max_x - self.min_x

    def get_height(self) -> float:
        return self.max_y - self.min_y

    def get_depth(self) -> float:
        return self.max_z - self.min_z

    def _calculate_surface_area(self) -> float:
        """Helper method to calculate surface area."""
        width = self.get_width()
        height = self.get_height()
        depth = self.get_depth()
        return 2.0 * (width * height + width * depth + height * depth)

    def overlaps(self, other: 'AABB') -> bool:
        """Check if this AABB overlaps with another."""
        # C++ order retained (y-check first)
        return (self.max_x > other.min_x and
                self.min_x < other.max_x and
                self.max_y > other.min_y and
                self.min_y < other.max_y and
                self.max_z > other.min_z and
                self.min_z < other.max_z)

    def contains(self, other: 'AABB') -> bool:
        """Check if this AABB completely contains another."""
        return (other.min_x >= self.min_x and
                other.max_x <= self.max_x and
                other.min_y >= self.min_y and
                other.max_y <= self.max_y and
                other.min_z >= self.min_z and
                other.max_z <= self.max_z)

    def merge(self, other: 'AABB') -> 'AABB':
        """Return a new AABB that encompasses both this and another AABB."""
        min_x = min(self.min_x, other.min_x)
        min_y = min(self.min_y, other.min_y)
        min_z = min(self.min_z, other.min_z)
        max_x = max(self.max_x, other.max_x)
        max_y = max(self.max_y, other.max_y)
        max_z = max(self.max_z, other.max_z)
        return AABB(min_x, min_y, min_z, max_x, max_y, max_z)

    def intersection(self, other: 'AABB') -> 'AABB':
        """Return a new AABB representing the intersection of this and another AABB."""
        min_x = max(self.min_x, other.min_x)
        min_y = max(self.min_y, other.min_y)
        min_z = max(self.min_z, other.min_z)
        max_x = min(self.max_x, other.max_x)
        max_y = min(self.max_y, other.max_y)
        max_z = min(self.max_z, other.max_z)

        # If there's no overlap, the intersection might have min > max.
        # Return an empty AABB (min=max=0 or similar) or handle as needed.
        # Here, we'll return it directly, but width/height/depth might be negative.
        # A robust implementation might check for valid intersection first.
        if min_x >= max_x or min_y >= max_y or min_z >= max_z:
             # Return a degenerate AABB or raise an error, depending on requirements
             return AABB() # Default empty AABB

        return AABB(min_x, min_y, min_z, max_x, max_y, max_z)

    def __repr__(self) -> str:
        """String representation for debugging."""
        return (f"AABB(min=({self.min_x:.2f}, {self.min_y:.2f}, {self.min_z:.2f}), "
                f"max=({self.max_x:.2f}, {self.max_y:.2f}, {self.max_z:.2f}))")


# --- AABBNode Class ---
class AABBNode:
    """Represents a node in the AABB Tree."""
    def __init__(self):
        self.aabb: AABB = AABB()  # The bounding box for this node
        self.object: Optional[IAABB] = None # Direct reference to the object if it's a leaf
        # Tree links (indices into the AABBTree._nodes list)
        self.parent_node_index: int = AABB_NULL_NODE
        self.left_node_index: int = AABB_NULL_NODE
        self.right_node_index: int = AABB_NULL_NODE
        # Free list link (index into the AABBTree._nodes list)
        self.next_node_index: int = AABB_NULL_NODE

    def is_leaf(self) -> bool:
        """Check if this node is a leaf node (no children)."""
        # A leaf node has no left child (and consequently no right child)
        return self.left_node_index == AABB_NULL_NODE

    def __repr__(self) -> str:
        obj_repr = f"Object({type(self.object).__name__})" if self.object else "None"
        return (f"Node(AABB={self.aabb}, Obj={obj_repr}, Parent={self.parent_node_index}, "
                f"L={self.left_node_index}, R={self.right_node_index}, NextFree={self.next_node_index})")


# --- AABBTree Class ---
class AABBTree:
    """
    A dynamic AABB Tree implementation for broad-phase collision detection.
    Uses a node pool and free list for efficient allocation.
    """
    def __init__(self, initial_size: int = 16):
        if initial_size <= 0:
            raise ValueError("Initial size must be positive")

        self._object_node_index_map: Dict[IAABB, int] = {}
        self._nodes: List[AABBNode] = []
        self._root_node_index: int = AABB_NULL_NODE
        self._allocated_node_count: int = 0
        self._next_free_node_index: int = 0
        self._node_capacity: int = initial_size
        self._growth_size: int = initial_size # How much to grow the node list by

        # Pre-allocate nodes and build the free list
        self._nodes = [AABBNode() for _ in range(initial_size)]
        for i in range(initial_size):
            self._nodes[i].next_node_index = i + 1
        # Mark the end of the free list
        self._nodes[initial_size - 1].next_node_index = AABB_NULL_NODE
        self._next_free_node_index = 0 # Start of the free list

    def _allocate_node(self) -> int:
        """Allocates a node from the free list, growing the pool if necessary."""
        if self._next_free_node_index == AABB_NULL_NODE:
            # Grow the node pool
            assert self._allocated_node_count == self._node_capacity
            old_capacity = self._node_capacity
            self._node_capacity += self._growth_size
            self._nodes.extend(AABBNode() for _ in range(self._growth_size))

            # Link the new nodes into the free list
            for i in range(old_capacity, self._node_capacity):
                 self._nodes[i].next_node_index = i + 1
            self._nodes[self._node_capacity - 1].next_node_index = AABB_NULL_NODE
            # The new free list starts where the old one ended (at the first new node)
            self._next_free_node_index = old_capacity

        # Get the next free node index
        node_index = self._next_free_node_index
        allocated_node = self._nodes[node_index]

        # Update the free list pointer
        self._next_free_node_index = allocated_node.next_node_index

        # Reset the allocated node's properties
        allocated_node.parent_node_index = AABB_NULL_NODE
        allocated_node.left_node_index = AABB_NULL_NODE
        allocated_node.right_node_index = AABB_NULL_NODE
        allocated_node.object = None
        # allocated_node.next_node_index is handled by free list logic

        self._allocated_node_count += 1
        return node_index

    def _deallocate_node(self, node_index: int):
        """Returns a node to the free list."""
        if not (0 <= node_index < self._node_capacity):
             raise IndexError("Node index out of bounds for deallocation")

        deallocated_node = self._nodes[node_index]
        # Link this node back to the start of the free list
        deallocated_node.next_node_index = self._next_free_node_index
        self._next_free_node_index = node_index
        self._allocated_node_count -= 1
        # Optionally clear other fields for safety/debugging
        # deallocated_node.object = None
        # deallocated_node.parent_node_index = AABB_NULL_NODE
        # ...

    def insert_object(self, obj: IAABB):
        """Inserts an object into the tree."""
        if obj in self._object_node_index_map:
            # Or raise an error/update instead? C++ didn't specify behavior.
            print(f"Warning: Object {obj} already in tree. Skipping insertion.")
            return

        node_index = self._allocate_node()
        node = self._nodes[node_index]

        node.aabb = obj.get_aabb()
        node.object = obj

        self._insert_leaf(node_index)
        self._object_node_index_map[obj] = node_index

    def remove_object(self, obj: IAABB):
        """Removes an object from the tree."""
        if obj not in self._object_node_index_map:
            # Or raise an error?
            print(f"Warning: Object {obj} not found in tree. Skipping removal.")
            return

        node_index = self._object_node_index_map[obj]
        self._remove_leaf(node_index)
        self._deallocate_node(node_index)
        del self._object_node_index_map[obj]

    def update_object(self, obj: IAABB):
        """Updates an object's position in the tree."""
        if obj not in self._object_node_index_map:
            # Or raise an error?
            print(f"Warning: Object {obj} not found in tree. Skipping update.")
            return

        node_index = self._object_node_index_map[obj]
        new_aabb = obj.get_aabb()
        self._update_leaf(node_index, new_aabb)


    def query_overlaps(self, obj: IAABB) -> List[IAABB]:
        """Finds all objects in the tree whose AABBs overlap with the given object's AABB."""
        overlaps: List[IAABB] = []
        stack: List[int] = [] # Use list as a stack (append/pop)
        test_aabb = obj.get_aabb()

        if self._root_node_index == AABB_NULL_NODE:
            return overlaps

        stack.append(self._root_node_index)
        while stack:
            node_index = stack.pop()

            if node_index == AABB_NULL_NODE:
                continue

            node = self._nodes[node_index]
            if node.aabb.overlaps(test_aabb):
                if node.is_leaf():
                    # Make sure we don't report collision with self
                    if node.object is not None and node.object != obj:
                         # C++ used push_front, Python append is more common (order reversed)
                         overlaps.append(node.object)
                else:
                    # Push children onto stack to check them next
                    stack.append(node.left_node_index)
                    stack.append(node.right_node_index)

        return overlaps # Note: order might differ from C++ forward_list::push_front

    # --- Private Helper Methods ---

    def _insert_leaf(self, leaf_node_index: int):
        """Inserts a leaf node into the tree structure."""
        leaf_node = self._nodes[leaf_node_index]
        assert leaf_node.is_leaf()
        assert leaf_node.parent_node_index == AABB_NULL_NODE

        # If tree is empty, the leaf becomes the root
        if self._root_node_index == AABB_NULL_NODE:
            self._root_node_index = leaf_node_index
            return

        # Find the best sibling for the new leaf using Surface Area Heuristic (SAH)
        tree_node_index = self._root_node_index
        while not self._nodes[tree_node_index].is_leaf():
            tree_node = self._nodes[tree_node_index] # Current internal node
            left_node_index = tree_node.left_node_index
            right_node_index = tree_node.right_node_index
            left_node = self._nodes[left_node_index]
            right_node = self._nodes[right_node_index]

            combined_aabb = tree_node.aabb.merge(leaf_node.aabb)

            # Cost of creating a new parent node here
            new_parent_node_cost = 2.0 * combined_aabb.surface_area

            # Minimum cost to push the leaf down further
            minimum_push_down_cost = 2.0 * (combined_aabb.surface_area - tree_node.aabb.surface_area)

            # Cost of descending left
            cost_left: float
            merged_left_aabb = leaf_node.aabb.merge(left_node.aabb)
            if left_node.is_leaf():
                cost_left = merged_left_aabb.surface_area + minimum_push_down_cost
            else:
                # Cost is increase in surface area
                cost_left = (merged_left_aabb.surface_area - left_node.aabb.surface_area) + minimum_push_down_cost

            # Cost of descending right
            cost_right: float
            merged_right_aabb = leaf_node.aabb.merge(right_node.aabb)
            if right_node.is_leaf():
                cost_right = merged_right_aabb.surface_area + minimum_push_down_cost
            else:
                # Cost is increase in surface area
                cost_right = (merged_right_aabb.surface_area - right_node.aabb.surface_area) + minimum_push_down_cost

            # If creating a new parent here is cheaper than descending, break
            if new_parent_node_cost < cost_left and new_parent_node_cost < cost_right:
                break

            # Otherwise, descend the cheaper branch
            if cost_left < cost_right:
                tree_node_index = left_node_index
            else:
                tree_node_index = right_node_index

        # Found the best sibling node (sibling_node_index)
        leaf_sibling_index = tree_node_index
        leaf_sibling = self._nodes[leaf_sibling_index]
        old_parent_index = leaf_sibling.parent_node_index

        # Create a new parent node
        new_parent_index = self._allocate_node()
        new_parent = self._nodes[new_parent_index]
        new_parent.parent_node_index = old_parent_index
        new_parent.aabb = leaf_node.aabb.merge(leaf_sibling.aabb)
        new_parent.left_node_index = leaf_sibling_index
        new_parent.right_node_index = leaf_node_index

        # Link the leaf and sibling to the new parent
        leaf_node.parent_node_index = new_parent_index
        leaf_sibling.parent_node_index = new_parent_index

        # Link the old parent (or root) to the new parent
        if old_parent_index == AABB_NULL_NODE:
            # The sibling was the root, so the new parent is now the root
            self._root_node_index = new_parent_index
        else:
            # The sibling was a child of old_parent
            old_parent = self._nodes[old_parent_index]
            if old_parent.left_node_index == leaf_sibling_index:
                old_parent.left_node_index = new_parent_index
            else:
                assert old_parent.right_node_index == leaf_sibling_index
                old_parent.right_node_index = new_parent_index

        # Fix AABBs and heights up the tree
        self._fix_upwards_tree(new_parent_index)


    def _remove_leaf(self, leaf_node_index: int):
        """Removes a leaf node from the tree structure."""
        assert self._nodes[leaf_node_index].is_leaf()

        # If the leaf is the root, just clear the root
        if leaf_node_index == self._root_node_index:
            self._root_node_index = AABB_NULL_NODE
            return

        leaf_node = self._nodes[leaf_node_index]
        parent_node_index = leaf_node.parent_node_index
        assert parent_node_index != AABB_NULL_NODE # Leaf must have a parent if not root
        parent_node = self._nodes[parent_node_index]
        grandparent_node_index = parent_node.parent_node_index

        # Find the sibling
        sibling_node_index: int
        if parent_node.left_node_index == leaf_node_index:
            sibling_node_index = parent_node.right_node_index
        else:
            sibling_node_index = parent_node.left_node_index
        assert sibling_node_index != AABB_NULL_NODE # Parent must have two children

        sibling_node = self._nodes[sibling_node_index]

        # Remove the parent node and promote the sibling
        if grandparent_node_index != AABB_NULL_NODE:
            # Parent is not the root
            grandparent_node = self._nodes[grandparent_node_index]
            # Connect grandparent to sibling
            if grandparent_node.left_node_index == parent_node_index:
                grandparent_node.left_node_index = sibling_node_index
            else:
                assert grandparent_node.right_node_index == parent_node_index
                grandparent_node.right_node_index = sibling_node_index

            # Connect sibling back to grandparent
            sibling_node.parent_node_index = grandparent_node_index
            self._deallocate_node(parent_node_index) # Free the parent node

            # Fix tree upwards starting from the grandparent
            self._fix_upwards_tree(grandparent_node_index)
        else:
            # Parent is the root, so sibling becomes the new root
            self._root_node_index = sibling_node_index
            sibling_node.parent_node_index = AABB_NULL_NODE # Sibling is now root
            self._deallocate_node(parent_node_index) # Free the old root (parent)
            # No need to fix upwards as the tree structure above root doesn't exist

        # Mark the removed leaf as having no parent (it's now detached)
        leaf_node.parent_node_index = AABB_NULL_NODE


    def _update_leaf(self, leaf_node_index: int, new_aabb: AABB):
        """Updates a leaf node's AABB, potentially restructuring the tree."""
        node = self._nodes[leaf_node_index]
        assert node.is_leaf()

        # TODO: Implement fattening/velocity check from C++ comment if needed
        # If the existing AABB already contains the new one, no structural change needed,
        # but we might still want to update the node's AABB to the tighter fit.
        # C++ version only updated if *not* contained. Let's match that for now.
        if node.aabb.contains(new_aabb):
             # Optional: Update to tighter AABB even if contained
             # node.aabb = new_aabb
             # self._fix_upwards_tree(node.parent_node_index)
            return # No structural change needed per C++ logic

        # AABB is outside the current node's box, remove and re-insert
        self._remove_leaf(leaf_node_index)
        node.aabb = new_aabb # Update the AABB *before* re-inserting
        self._insert_leaf(leaf_node_index)


    def _fix_upwards_tree(self, tree_node_index: int):
        """Walks up the tree from a node, fixing AABBs."""
        while tree_node_index != AABB_NULL_NODE:
            tree_node = self._nodes[tree_node_index]

            # Should not be called on a leaf node after insertion/removal logic
            assert not tree_node.is_leaf()
            assert tree_node.left_node_index != AABB_NULL_NODE
            assert tree_node.right_node_index != AABB_NULL_NODE

            # Recalculate AABB based on children
            left_node = self._nodes[tree_node.left_node_index]
            right_node = self._nodes[tree_node.right_node_index]
            tree_node.aabb = left_node.aabb.merge(right_node.aabb)

            # Move up to the parent
            tree_node_index = tree_node.parent_node_index

# --- Example Usage (Optional) ---
if __name__ == '__main__':
    # Example implementation of IAABB
    class Box(IAABB):
        _id_counter = 0
        def __init__(self, x, y, z, w, h, d):
            self.x, self.y, self.z = x, y, z
            self.w, self.h, self.d = w, h, d
            self.id = Box._id_counter
            Box._id_counter += 1

        def get_aabb(self) -> AABB:
            half_w, half_h, half_d = self.w / 2, self.h / 2, self.d / 2
            return AABB(self.x - half_w, self.y - half_h, self.z - half_d,
                        self.x + half_w, self.y + half_h, self.z + half_d)

        # Need __eq__ and __hash__ if we want to use Box objects as dict keys reliably
        def __eq__(self, other):
            if not isinstance(other, Box):
                return NotImplemented
            return self.id == other.id

        def __hash__(self):
            return hash(self.id)

        def __repr__(self):
            return f"Box(id={self.id}, pos=({self.x},{self.y},{self.z}))"

    # Create tree
    tree = AABBTree(initial_size=4)

    # Create some objects
    box1 = Box(0, 0, 0, 2, 2, 2)  # Centered at origin, size 2x2x2
    box2 = Box(1.5, 0, 0, 2, 2, 2) # Slightly offset, should overlap box1
    box3 = Box(5, 5, 5, 1, 1, 1)  # Far away

    # Insert objects
    tree.insert_object(box1)
    print(f"Inserted {box1}")
    tree.insert_object(box2)
    print(f"Inserted {box2}")
    tree.insert_object(box3)
    print(f"Inserted {box3}")

    # Query overlaps for box1
    print(f"\nQuerying overlaps for {box1}:")
    overlaps1 = tree.query_overlaps(box1)
    for obj in overlaps1:
        print(f"  - Overlaps with {obj}") # Should report box2

    # Query overlaps for box3
    print(f"\nQuerying overlaps for {box3}:")
    overlaps3 = tree.query_overlaps(box3)
    if not overlaps3:
        print("  - No overlaps found (Correct)")
    else:
         for obj in overlaps3:
            print(f"  - Overlaps with {obj}")

    # Update box3 to overlap box1
    print(f"\nUpdating {box3} position...")
    box3.x, box3.y, box3.z = 0.5, 0.5, 0.5
    tree.update_object(box3)

    # Query overlaps for box1 again
    print(f"\nQuerying overlaps for {box1} after update:")
    overlaps1_updated = tree.query_overlaps(box1)
    for obj in overlaps1_updated:
        print(f"  - Overlaps with {obj}") # Should report box2 and box3

    # Remove box2
    print(f"\nRemoving {box2}...")
    tree.remove_object(box2)

    # Query overlaps for box1 again
    print(f"\nQuerying overlaps for {box1} after removal:")
    overlaps1_removed = tree.query_overlaps(box1)
    for obj in overlaps1_removed:
        print(f"  - Overlaps with {obj}") # Should report only box3

    print("\nTree state (nodes):")
    # Be careful printing large trees
    # for i, node in enumerate(tree._nodes):
    #     if i < tree._node_capacity: # Only print allocated/potentially used nodes
    #         print(f"  Node {i}: {node}")
