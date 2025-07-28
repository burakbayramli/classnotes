import * as THREE from 'three';
import { GUI } from 'three/examples/jsm/libs/lil-gui.module.min.js';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';
import { STLLoader } from 'three/examples/jsm/loaders/STLLoader.js';


let camera, scene, renderer;
let cameraControls;
let ambientLight, light;

init();

function init() {
    console.log(THREE.REVISION);

    const container = document.createElement( 'div' );
    document.body.appendChild( container );

    const canvasWidth = window.innerWidth;
    const canvasHeight = window.innerHeight;

    camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
    camera.position.set(0, 0, 5);

    ambientLight = new THREE.AmbientLight( 0x7c7c7c, 2.0 );

    light = new THREE.DirectionalLight( 0xFFFFFF, 2.0 );
    light.position.set( 0.32, 0.39, 0.7 );

    renderer = new THREE.WebGLRenderer( { antialias: true } );
    renderer.setPixelRatio( window.devicePixelRatio );
    renderer.setSize( canvasWidth, canvasHeight );

    container.appendChild( renderer.domElement );

    const raycaster = new THREE.Raycaster();
    const pointer = new THREE.Vector2();

    function onPointerMove( event ) {
	pointer.x = ( event.clientX / window.innerWidth ) * 2 - 1;
	pointer.y = - ( event.clientY / window.innerHeight ) * 2 + 1;
    }
    window.addEventListener("keydown", (e) => {
        if(e.code == 'KeyR'){
	    console.log("pressed");

	    raycaster.setFromCamera( pointer, camera );

	    const intersects = raycaster.intersectObjects( scene.children );
	    if (intersects.length > 0) {
		console.log(intersects[0].point);
		var sphereGeometry = new THREE.SphereGeometry(10, 10, 10);
		var sphereMaterial = new THREE.MeshLambertMaterial({
		    color: 0x7777ff
		});
		var sphere = new THREE.Mesh(sphereGeometry, sphereMaterial);
		sphere.position.x = intersects[0].point.x;
		sphere.position.y = intersects[0].point.y;
		sphere.position.z = intersects[0].point.z;
		sphere.castShadow = true;
		scene.add(sphere);

		const diffx = camera.position.x - intersects[0].point.x;
		const diffy = camera.position.y - intersects[0].point.y;
		const diffz = camera.position.z - intersects[0].point.z;
		const dir = new THREE.Vector3(diffx, diffy, diffz).normalize();
		const origin = new THREE.Vector3( intersects[0].point.x, intersects[0].point.y, intersects[0].point.z );
		const length = 1;
		const hex = 0xffff00;
		const arrowHelper = new THREE.ArrowHelper( dir, origin, length, hex );
		scene.add( arrowHelper );

		render();
	    }
	}
    });

    window.addEventListener( 'pointermove', onPointerMove );

    cameraControls = new OrbitControls( camera, renderer.domElement );
    cameraControls.addEventListener( 'change', render );
    cameraControls.update(); // Important for initial state after target change

    const path = 'textures/cube/pisa/';
    const urls = [ 'px.png', 'nx.png', 'py.png', 'ny.png', 'pz.png', 'nz.png' ];

    scene = new THREE.Scene();
    scene.background = new THREE.Color( 0xAAAAAA );

    scene.add( ambientLight );
    scene.add( light );
    createNewTorus();
}

function render() {

    renderer.render( scene, camera );

}

function createNewTorus() {

    const loader = new STLLoader();

    loader.load( 'https://burakbayramli.github.io/dersblog/sk/2020/08/shapes/Prism_hexagon.stl', function ( geometry ) {

	geometry.computeBoundingBox();
	const boundingBox = geometry.boundingBox;
	const center = new THREE.Vector3();
	boundingBox.getCenter(center);
	geometry.translate(-center.x, -center.y, -center.z);

	geometry.computeBoundingBox();
	const newBoundingBox = geometry.boundingBox;
	const size = new THREE.Vector3();
	newBoundingBox.getSize(size);

	const maxDimension = Math.max(size.x, size.y, size.z);
	const targetSize = 2; // Desired size in world units
	const scaleFactor = targetSize / maxDimension;

	const material = new THREE.MeshPhongMaterial({ color: 0xff9c7c, specular: 0x494949, shininess: 200 });
	const mesh = new THREE.Mesh(geometry, material);

	mesh.scale.set(scaleFactor, scaleFactor, scaleFactor);

	const distanceInFrontOfCamera = 5; // Adjust this value as needed. 5 is a good starting point for targetSize=2
	mesh.position.set(camera.position.x, camera.position.y, camera.position.z - distanceInFrontOfCamera);


	mesh.castShadow = true;
	mesh.receiveShadow = true;
	scene.add(mesh);

    cameraControls.target.copy(mesh.position);
    cameraControls.update();


	render();
    } );
}
