// Represents an object that can provide its Axis-Aligned Bounding Box.
// In JS, this is more conceptual (duck typing). Objects passed to AABBTree
// MUST have a getAABB() method returning an AABB instance.
class IAABB {
    /**
     * @returns {AABB} The bounding box for this object.
     */
    getAABB() {
        throw new Error("Method 'getAABB()' must be implemented.");
    }
}

const AABB_NULL_NODE = -1; // Convention for invalid node index

class AABB {
    minX = 0.0;
    minY = 0.0;
    minZ = 0.0;
    maxX = 0.0;
    maxY = 0.0;
    maxZ = 0.0;
    surfaceArea = 0.0;

    /**
     * Private method to calculate surface area.
     * @returns {number}
     */
    #calculateSurfaceArea() {
        const width = this.getWidth();
        const height = this.getHeight();
        const depth = this.getDepth();
        return 2.0 * (width * height + width * depth + height * depth);
    }

    constructor(minX = 0.0, minY = 0.0, minZ = 0.0, maxX = 0.0, maxY = 0.0, maxZ = 0.0) {
        // JS doesn't have direct constructor overloading based on types like C++.
        // We assume numbers are passed.
        this.minX = minX;
        this.minY = minY;
        this.minZ = minZ;
        this.maxX = maxX;
        this.maxY = maxY;
        this.maxZ = maxZ;
        this.surfaceArea = this.#calculateSurfaceArea();
    }

    /**
     * Checks if this AABB overlaps with another AABB.
     * @param {AABB} other The other AABB.
     * @returns {boolean} True if they overlap, false otherwise.
     */
    overlaps(other) {
        // y is deliberately first as per original comment
        return this.maxX > other.minX &&
               this.minX < other.maxX &&
               this.maxY > other.minY &&
               this.minY < other.maxY &&
               this.maxZ > other.minZ &&
               this.minZ < other.maxZ;
    }

    /**
     * Checks if this AABB completely contains another AABB.
     * @param {AABB} other The other AABB.
     * @returns {boolean} True if this contains other, false otherwise.
     */
    contains(other) {
        return other.minX >= this.minX &&
               other.maxX <= this.maxX &&
               other.minY >= this.minY &&
               other.maxY <= this.maxY &&
               other.minZ >= this.minZ &&
               other.maxZ <= this.maxZ;
    }

    /**
     * Creates a new AABB that encompasses both this AABB and another one.
     * @param {AABB} other The other AABB.
     * @returns {AABB} A new AABB representing the union.
     */
    merge(other) {
        return new AABB(
            Math.min(this.minX, other.minX), Math.min(this.minY, other.minY), Math.min(this.minZ, other.minZ),
            Math.max(this.maxX, other.maxX), Math.max(this.maxY, other.maxY), Math.max(this.maxZ, other.maxZ)
        );
    }

    /**
     * Creates a new AABB representing the intersection of this AABB and another one.
     * Assumes they overlap; otherwise, the result might be invalid (min > max).
     * @param {AABB} other The other AABB.
     * @returns {AABB} A new AABB representing the intersection.
     */
    intersection(other) {
         return new AABB(
            Math.max(this.minX, other.minX), Math.max(this.minY, other.minY), Math.max(this.minZ, other.minZ),
            Math.min(this.maxX, other.maxX), Math.min(this.maxY, other.maxY), Math.min(this.maxZ, other.maxZ)
         );
    }

    /** @returns {number} The width (X-axis extent). */
    getWidth() { return this.maxX - this.minX; }
    /** @returns {number} The height (Y-axis extent). */
    getHeight() { return this.maxY - this.minY; }
    /** @returns {number} The depth (Z-axis extent). */
    getDepth() { return this.maxZ - this.minZ; }
}

class AABBNode {
    aabb = new AABB(); // The bounding box for this node
    object = null;     // The actual object (IAABB), only for leaf nodes

    // Tree links (indices into the AABBTree's nodes array)
    parentNodeIndex = AABB_NULL_NODE;
    leftNodeIndex = AABB_NULL_NODE;
    rightNodeIndex = AABB_NULL_NODE;

    // Link for the free list
    nextNodeIndex = AABB_NULL_NODE;

    /**
     * Checks if this node is a leaf node (contains an object).
     * @returns {boolean}
     */
    isLeaf() {
        return this.leftNodeIndex === AABB_NULL_NODE;
        // Assumption: If left is null, right must also be null for a leaf.
        // Internal nodes always have both children.
    }

    constructor() {
        // Default initialization happens with property initializers above
    }
}


class AABBTree {
    #objectNodeIndexMap = new Map(); // Map<IAABB, number> (object -> node index)
    #nodes = [];                     // Array<AABBNode>
    #rootNodeIndex = AABB_NULL_NODE;
    #allocatedNodeCount = 0;
    #nextFreeNodeIndex = AABB_NULL_NODE;
    #nodeCapacity = 0;
    #growthSize = 0; // How much to grow the node pool by

    /**
     * Allocates a node from the free list or grows the pool if necessary.
     * @returns {number} The index of the allocated node.
     */
    #allocateNode() {
        // If we have no free tree nodes, grow the pool
        if (this.#nextFreeNodeIndex === AABB_NULL_NODE) {
            console.assert(this.#allocatedNodeCount === this.#nodeCapacity, "Node pool inconsistency");

            const oldCapacity = this.#nodeCapacity;
            this.#nodeCapacity += this.#growthSize;
            this.#nodes.length = this.#nodeCapacity; // Resize array

            // Initialize new nodes and link them into the free list
            for (let i = oldCapacity; i < this.#nodeCapacity; i++) {
                this.#nodes[i] = new AABBNode(); // Create the new node object
                this.#nodes[i].nextNodeIndex = i + 1;
            }
            // Last new node points to null
            this.#nodes[this.#nodeCapacity - 1].nextNodeIndex = AABB_NULL_NODE;
            // Set the start of the free list to the first new node
            this.#nextFreeNodeIndex = oldCapacity;
        }

        // Get the next free node index
        const nodeIndex = this.#nextFreeNodeIndex;
        const allocatedNode = this.#nodes[nodeIndex];

        // Update the free list pointer
        this.#nextFreeNodeIndex = allocatedNode.nextNodeIndex;

        // Reset the allocated node's properties
        allocatedNode.parentNodeIndex = AABB_NULL_NODE;
        allocatedNode.leftNodeIndex = AABB_NULL_NODE;
        allocatedNode.rightNodeIndex = AABB_NULL_NODE;
        allocatedNode.nextNodeIndex = AABB_NULL_NODE; // Not strictly needed but good practice
        allocatedNode.object = null;
        // allocatedNode.aabb will be set by the caller

        this.#allocatedNodeCount++;
        return nodeIndex;
    }

    /**
     * Returns a node to the free list.
     * @param {number} nodeIndex The index of the node to deallocate.
     */
    #deallocateNode(nodeIndex) {
        if (nodeIndex < 0 || nodeIndex >= this.#nodeCapacity) {
             console.error("Invalid node index for deallocation:", nodeIndex);
             return;
        }
        const deallocatedNode = this.#nodes[nodeIndex];
        deallocatedNode.nextNodeIndex = this.#nextFreeNodeIndex; // Point to the current head of the free list
        this.#nextFreeNodeIndex = nodeIndex; // Make this node the new head
        this.#allocatedNodeCount--;
    }

    /**
     * Inserts a leaf node into the tree. Finds the best position based on SAH.
     * @param {number} leafNodeIndex The index of the new leaf node.
     */
    #insertLeaf(leafNodeIndex) {
        const leafNode = this.#nodes[leafNodeIndex];
        // Ensure it's a new leaf
        console.assert(leafNode.parentNodeIndex === AABB_NULL_NODE, "Leaf already has parent");
        console.assert(leafNode.isLeaf(), "Node to insert is not a leaf");

        // If the tree is empty, make the leaf the root
        if (this.#rootNodeIndex === AABB_NULL_NODE) {
            this.#rootNodeIndex = leafNodeIndex;
            return;
        }

        // Find the best sibling for the new leaf
        let treeNodeIndex = this.#rootNodeIndex;
        let bestSiblingIndex = treeNodeIndex; // Keep track if we break early

        while (!this.#nodes[treeNodeIndex].isLeaf()) {
            const treeNode = this.#nodes[treeNodeIndex];
            const leftNodeIndex = treeNode.leftNodeIndex;
            const rightNodeIndex = treeNode.rightNodeIndex;
            const leftNode = this.#nodes[leftNodeIndex];
            const rightNode = this.#nodes[rightNodeIndex];

            const combinedAabb = treeNode.aabb.merge(leafNode.aabb);
            const currentSurfaceArea = treeNode.aabb.surfaceArea;

            const newParentNodeCost = 2.0 * combinedAabb.surfaceArea;
            const minimumPushDownCost = 2.0 * (combinedAabb.surfaceArea - currentSurfaceArea);

            // Cost of descending to the left
            let costLeft;
            const mergedLeftAABB = leafNode.aabb.merge(leftNode.aabb);
            if (leftNode.isLeaf()) {
                costLeft = mergedLeftAABB.surfaceArea + minimumPushDownCost;
            } else {
                costLeft = (mergedLeftAABB.surfaceArea - leftNode.aabb.surfaceArea) + minimumPushDownCost;
            }

            // Cost of descending to the right
            let costRight;
            const mergedRightAABB = leafNode.aabb.merge(rightNode.aabb);
            if (rightNode.isLeaf()) {
                costRight = mergedRightAABB.surfaceArea + minimumPushDownCost;
            } else {
                costRight = (mergedRightAABB.surfaceArea - rightNode.aabb.surfaceArea) + minimumPushDownCost;
            }

            // If the cost of creating a new parent here is less than descending, break
            if (newParentNodeCost < costLeft && newParentNodeCost < costRight) {
                bestSiblingIndex = treeNodeIndex;
                break;
            }

            // Otherwise, descend to the cheaper child
            if (costLeft < costRight) {
                treeNodeIndex = leftNodeIndex;
            } else {
                treeNodeIndex = rightNodeIndex;
            }
             bestSiblingIndex = treeNodeIndex; // Update best sibling in case we exit loop
        }

        // Create a new parent node
        const leafSiblingIndex = bestSiblingIndex; // The node found becomes the sibling
        const leafSibling = this.#nodes[leafSiblingIndex];
        const oldParentIndex = leafSibling.parentNodeIndex;
        const newParentIndex = this.#allocateNode();
        const newParent = this.#nodes[newParentIndex];

        newParent.parentNodeIndex = oldParentIndex;
        newParent.aabb = leafNode.aabb.merge(leafSibling.aabb); // New parent's AABB
        newParent.leftNodeIndex = leafSiblingIndex;
        newParent.rightNodeIndex = leafNodeIndex;

        leafNode.parentNodeIndex = newParentIndex;
        leafSibling.parentNodeIndex = newParentIndex;

        // Link the new parent into the tree
        if (oldParentIndex === AABB_NULL_NODE) {
            // The sibling was the root, so the new parent is now the root
            this.#rootNodeIndex = newParentIndex;
        } else {
            // The sibling was not the root, update the old parent's child pointer
            const oldParent = this.#nodes[oldParentIndex];
            if (oldParent.leftNodeIndex === leafSiblingIndex) {
                oldParent.leftNodeIndex = newParentIndex;
            } else {
                console.assert(oldParent.rightNodeIndex === leafSiblingIndex, "Sibling mismatch");
                oldParent.rightNodeIndex = newParentIndex;
            }
        }

        // Walk back up the tree, fixing AABBs
        this.#fixUpwardsTree(newParentIndex);
    }

    /**
     * Removes a leaf node from the tree.
     * @param {number} leafNodeIndex The index of the leaf node to remove.
     */
    #removeLeaf(leafNodeIndex) {
        // If the leaf is the root, just clear the root
        if (leafNodeIndex === this.#rootNodeIndex) {
            this.#rootNodeIndex = AABB_NULL_NODE;
            return;
        }

        const leafNode = this.#nodes[leafNodeIndex];
        const parentNodeIndex = leafNode.parentNodeIndex;
        const parentNode = this.#nodes[parentNodeIndex];
        const grandParentNodeIndex = parentNode.parentNodeIndex;
        const siblingNodeIndex = (parentNode.leftNodeIndex === leafNodeIndex)
                               ? parentNode.rightNodeIndex
                               : parentNode.leftNodeIndex;

        console.assert(siblingNodeIndex !== AABB_NULL_NODE, "Leaf must have a sibling");
        const siblingNode = this.#nodes[siblingNodeIndex];

        if (grandParentNodeIndex !== AABB_NULL_NODE) {
            // If there's a grandparent, connect the sibling to the grandparent
            const grandParentNode = this.#nodes[grandParentNodeIndex];
            if (grandParentNode.leftNodeIndex === parentNodeIndex) {
                grandParentNode.leftNodeIndex = siblingNodeIndex;
            } else {
                 console.assert(grandParentNode.rightNodeIndex === parentNodeIndex, "Grandparent connection error");
                grandParentNode.rightNodeIndex = siblingNodeIndex;
            }
            siblingNode.parentNodeIndex = grandParentNodeIndex;
            this.#deallocateNode(parentNodeIndex); // Remove the old parent

            // Fix AABBs upwards from the grandparent
            this.#fixUpwardsTree(grandParentNodeIndex);
        } else {
            // If there's no grandparent, the parent was the root. The sibling becomes the new root.
            this.#rootNodeIndex = siblingNodeIndex;
            siblingNode.parentNodeIndex = AABB_NULL_NODE;
            this.#deallocateNode(parentNodeIndex); // Remove the old parent (which was the root)
            // No need to fix upwards as the sibling was already correct
        }

        leafNode.parentNodeIndex = AABB_NULL_NODE; // Mark the removed leaf as detached
        // The leaf node itself will be deallocated by the caller (removeObject)
    }

     /**
     * Updates a leaf node's AABB. If the new AABB doesn't fit, removes and reinserts.
     * @param {number} leafNodeIndex The index of the leaf node to update.
     * @param {AABB} newAabb The new AABB for the object.
     */
    #updateLeaf(leafNodeIndex, newAabb) {
        const node = this.#nodes[leafNodeIndex];

        // If the new AABB is already contained within the node's current AABB, do nothing.
        // This is an optimization, especially if AABBs are slightly expanded (e.g., for velocity).
        if (node.aabb.contains(newAabb)) {
            return;
        }

        // Otherwise, the tree structure might need to change
        this.#removeLeaf(leafNodeIndex);
        node.aabb = newAabb; // Update the AABB *after* removing
        this.#insertLeaf(leafNodeIndex); // Reinsert the node with the new AABB
    }


    /**
     * Walks up the tree from a node, recalculating AABBs.
     * @param {number} treeNodeIndex The starting node index.
     */
    #fixUpwardsTree(treeNodeIndex) {
        while (treeNodeIndex !== AABB_NULL_NODE) {
            const treeNode = this.#nodes[treeNodeIndex];

             // Internal nodes must have two children after insertion/removal logic
             console.assert(treeNode.leftNodeIndex !== AABB_NULL_NODE && treeNode.rightNodeIndex !== AABB_NULL_NODE, "Internal node missing child");

            // Recalculate AABB based on children
            const leftNode = this.#nodes[treeNode.leftNodeIndex];
            const rightNode = this.#nodes[treeNode.rightNodeIndex];
            treeNode.aabb = leftNode.aabb.merge(rightNode.aabb);

            // Move up to the parent
            treeNodeIndex = treeNode.parentNodeIndex;
        }
    }

    /**
     * Creates an AABB Tree.
     * @param {number} initialSize The initial capacity of the node pool.
     */
    constructor(initialSize) {
        this.#nodeCapacity = initialSize;
        this.#growthSize = initialSize > 0 ? initialSize : 16; // Default growth size
        this.#nodes = new Array(initialSize);
        this.#allocatedNodeCount = 0;
        this.#rootNodeIndex = AABB_NULL_NODE;

        // Initialize the nodes and link them into a free list
        for (let i = 0; i < initialSize; i++) {
            this.#nodes[i] = new AABBNode();
            this.#nodes[i].nextNodeIndex = i + 1;
        }
        if (initialSize > 0) {
            this.#nodes[initialSize - 1].nextNodeIndex = AABB_NULL_NODE;
            this.#nextFreeNodeIndex = 0; // Start of the free list
        } else {
             this.#nextFreeNodeIndex = AABB_NULL_NODE;
        }
    }

    /**
     * Inserts an object into the tree. The object must have a getAABB() method.
     * @param {IAABB} object The object to insert.
     */
    insertObject(object) {
        const nodeIndex = this.#allocateNode();
        const node = this.#nodes[nodeIndex];

        node.aabb = object.getAABB();
        node.object = object; // Store the actual object in the leaf

        this.#insertLeaf(nodeIndex);
        this.#objectNodeIndexMap.set(object, nodeIndex); // Map object to its node index
    }

    /**
     * Removes an object from the tree.
     * @param {IAABB} object The object to remove.
     */
    removeObject(object) {
        if (!this.#objectNodeIndexMap.has(object)) {
            console.warn("Attempting to remove object not in tree:", object);
            return;
        }
        const nodeIndex = this.#objectNodeIndexMap.get(object);

        this.#removeLeaf(nodeIndex);
        this.#deallocateNode(nodeIndex);
        this.#objectNodeIndexMap.delete(object);
    }

    /**
     * Updates an object's position in the tree. Call this if the object moves.
     * @param {IAABB} object The object to update.
     */
    updateObject(object) {
         if (!this.#objectNodeIndexMap.has(object)) {
            console.warn("Attempting to update object not in tree:", object);
            // Optionally, insert it if desired behavior:
            // this.insertObject(object);
            return;
        }
        const nodeIndex = this.#objectNodeIndexMap.get(object);
        const newAabb = object.getAABB();
        this.#updateLeaf(nodeIndex, newAabb);
    }

    /**
     * Queries the tree for objects whose AABBs overlap with the query object's AABB.
     * @param {IAABB} queryObj The object (or an object with a getAABB()) to query against.
     * @returns {Array<IAABB>} A list of objects that overlap.
     */
    queryOverlaps(queryObj) {
        const overlaps = []; // Use array as list
        const stack = [];    // Use array as stack
        const testAabb = queryObj.getAABB();

        if (this.#rootNodeIndex === AABB_NULL_NODE) {
            return overlaps;
        }

        stack.push(this.#rootNodeIndex);

        while (stack.length > 0) {
            const nodeIndex = stack.pop();

            if (nodeIndex === AABB_NULL_NODE) continue; // Should not happen with proper checks

            const node = this.#nodes[nodeIndex];

            if (node.aabb.overlaps(testAabb)) {
                if (node.isLeaf()) {
                     // Don't report collision with self if the query object is in the tree
                    if (node.object !== queryObj) {
                       overlaps.push(node.object); // Add object to results
                    }
                } else {
                    // Push children onto stack to check them
                    stack.push(node.leftNodeIndex);
                    stack.push(node.rightNodeIndex);
                }
            }
        }
        return overlaps;
    }

     /**
     * (Optional) Gets the AABB of the root node, representing the entire tree bounds.
     * @returns {AABB | null} The root AABB or null if the tree is empty.
     */
    getRootAABB() {
        if (this.#rootNodeIndex !== AABB_NULL_NODE) {
            return this.#nodes[this.#rootNodeIndex].aabb;
        }
        return null;
    }

    /**
     * (Optional) Clears the tree and resets the node pool.
     */
    clear() {
        this.#objectNodeIndexMap.clear();
        this.#allocatedNodeCount = 0;
        this.#rootNodeIndex = AABB_NULL_NODE;

        // Re-initialize the free list
        for (let i = 0; i < this.#nodeCapacity; i++) {
            // Reset node properties (optional, but good practice)
             const node = this.#nodes[i];
             node.parentNodeIndex = AABB_NULL_NODE;
             node.leftNodeIndex = AABB_NULL_NODE;
             node.rightNodeIndex = AABB_NULL_NODE;
             node.object = null;
             // Link into free list
             node.nextNodeIndex = i + 1;
        }
         if (this.#nodeCapacity > 0) {
            this.#nodes[this.#nodeCapacity - 1].nextNodeIndex = AABB_NULL_NODE;
            this.#nextFreeNodeIndex = 0;
        } else {
             this.#nextFreeNodeIndex = AABB_NULL_NODE;
        }
    }
}


// --- Example Usage ---

// 1. Define a class for your objects that implements getAABB()
class MyGameObject extends IAABB {
    constructor(id, x, y, z, width, height, depth) {
        super(); // Necessary when extending
        this.id = id;
        this.x = x;
        this.y = y;
        this.z = z;
        this.width = width;
        this.height = height;
        this.depth = depth;
        this.aabb = null; // Cache AABB
        this.updateAABB();
    }

    updateAABB() {
         const halfW = this.width / 2;
         const halfH = this.height / 2;
         const halfD = this.depth / 2;
         this.aabb = new AABB(
             this.x - halfW, this.y - halfH, this.z - halfD,
             this.x + halfW, this.y + halfH, this.z + halfD
         );
    }

    // Implement the required method
    getAABB() {
        return this.aabb;
    }

    move(dx, dy, dz) {
        this.x += dx;
        this.y += dy;
        this.z += dz;
        this.updateAABB(); // Important: Update AABB after moving
    }
}

// 2. Create the tree
const tree = new AABBTree(16); // Initial capacity of 16 nodes

// 3. Create objects
const obj1 = new MyGameObject("box1", 0, 0, 0, 2, 2, 2);
const obj2 = new MyGameObject("box2", 5, 0, 0, 2, 2, 2);
const obj3 = new MyGameObject("box3", 1, 1, 1, 1, 1, 1); // Overlaps with obj1

// 4. Insert objects
tree.insertObject(obj1);
tree.insertObject(obj2);
tree.insertObject(obj3);

// 5. Query for overlaps
console.log("Querying overlaps for obj1:");
const overlaps1 = tree.queryOverlaps(obj1);
overlaps1.forEach(obj => console.log(" - Found:", obj.id)); // Should find obj3

console.log("\nQuerying overlaps for obj2:");
const overlaps2 = tree.queryOverlaps(obj2);
overlaps2.forEach(obj => console.log(" - Found:", obj.id)); // Should find none

// 6. Move an object and update the tree
console.log("\nMoving obj3 to overlap obj2...");
obj3.move(5, 0, 0); // Move obj3 to (6, 1, 1)
tree.updateObject(obj3); // Crucial step!

console.log("\nQuerying overlaps for obj2 after moving obj3:");
const overlaps3 = tree.queryOverlaps(obj2);
overlaps3.forEach(obj => console.log(" - Found:", obj.id)); // Should find obj3 now

// 7. Remove an object
console.log("\nRemoving obj1...");
tree.removeObject(obj1);

console.log("\nQuerying overlaps for obj3 after removing obj1:");
const overlaps4 = tree.queryOverlaps(obj3);
overlaps4.forEach(obj => console.log(" - Found:", obj.id)); // Should find obj2
