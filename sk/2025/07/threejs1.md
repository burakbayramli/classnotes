# Javascript ile Animasyon ThreeJS

https://github.com/mrdoob/three.js

0.178.0

```
./examples/jsm/loaders/STLLoader.js
./examples/jsm/libs/lil-gui.module.min.js
./examples/jsm/controls/OrbitControls.js
./build/three.core.js
./build/three.module.js
```

Three.js kullanarak temel bir 3D sahne oluşturur, bir STL modeli
yükler ve bazı etkileşimlere izin verir. Three.js'e yeni başlayanlar
için önemli kavramları ve potansiyel "gotcha"ları vurgulayarak aşağıda
bir dökümünü bulabilirsiniz:

Temel Kavramlar

1.  Sahne (Scene), Kamera (Camera), İşleyici (Renderer): Bunlar, herhangi bir Three.js uygulamasının temel bileşenleridir.

      * `Scene` (Sahne): Tüm 3D nesnelerinizi, ışıklarınızı ve kameralarınızı yerleştirdiğiniz yer burasıdır. Nesnelerinizin yaşadığı dünya olarak düşünebilirsiniz.
      * `Camera` (Kamera): Sahnelerinizi bu şekilde görüntülersiniz. Kod, insan gözünün nesneleri nasıl gördüğünü (uzaktaki nesnelerin daha küçük görünmesi gibi) simüle eden `THREE.PerspectiveCamera` kullanır.
      * `Renderer` (İşleyici): Bu, `Scene` ve `Camera`'yı alır ve görüntüyü web sayfanızın `<canvas>` öğesine işler. `THREE.WebGLRenderer`, verimli işleme için bilgisayarınızın grafik kartını kullanır.

2.  Işıklar (Lights):

      * `AmbientLight` (Ortam Işığı): Bu ışık, sahnedeki tüm nesneleri tüm yönlerden eşit şekilde aydınlatır. Gölge oluşturmaz ve genellikle temel bir aydınlatma seviyesi sağlamak için kullanılır.
      * `DirectionalLight` (Yönlü Işık): Güneş gibi uzak bir kaynaktan gelen ışığı simüle eder. Tüm ışık ışınları paraleldir. Bir konumu vardır ancak ışınları her zaman aynı yöne bakar. Gölgeler oluşturmak için iyidir.

3.  Nesneler (`Mesh`, `Geometry`, `Material`):

      * `Mesh` (Örgü): `Mesh`, sahnenizde gördüğünüz gerçek nesnedir. Bir `Geometry` ve bir `Material` kombinasyonudur.
      * `Geometry` (Geometri): 3D nesnenizin şeklini tanımlar (örn. bir küre, bir kutu veya STL gibi yüklü bir 3D model).
      * `Material` (Malzeme): Nesnenizin görünümünü (rengi, ne kadar parlak olduğu, ışığa nasıl tepki verdiği vb.) tanımlar. Kod, ışığa tepki veren ve yansımaları gösterebilen parlak, metalik görünümlü nesneler için iyi olan `MeshPhongMaterial` kullanır.

4.  **3D Modelleri Yükleme (`STLLoader`):**

      * Kod, STL formatında bir 3D model yüklemek için `STLLoader` kullanır (`Prism_hexagon.stl`). Bu, 3D baskı ve CAD yazılımı için yaygın bir formattır.
      * `loader.load()` fonksiyonu asenkrondur, yani modeli tarayıcınızı dondurmadan arka planda yükler. Yüklenen `geometry` daha sonra bir geri arama fonksiyonuna iletilir.

5.  **Işın Dökümü (`Raycaster`):**

      * `Raycaster`, 3D sahnenizdeki hangi nesnelerin belirli bir noktadan (fare imleciniz gibi) çıkan bir "ışın" tarafından kesiştiğini tespit etmek için kullanılan güçlü bir araçtır.
      * Bu kodda, 'R' tuşuna basıldığında fare imlecinizin altındaki 3D sahnedeki noktayı bulmak için kullanılır.

6.  **Kullanıcı Etkileşimi (`OrbitControls`, Olay Dinleyicileri):**

      * **`OrbitControls`**: Bu, farenizi kullanarak kameranızı bir hedef noktanın etrafında kolayca döndürmenizi, kaydırmanızı ve yakınlaştırmanızı sağlayan Three.js için çok kullanışlı bir yardımcı programdır. 3D sahnelerde gezinmek için vazgeçilmezdir.
      * **Olay Dinleyicileri (Event Listeners)**: `window.addEventListener('keydown', ...)` ve `window.addEventListener('pointermove', ...)` sırasıyla klavye tuş basmalarını ve fare hareketlerini algılamak için kullanılır.

Kod İncelemesi

#### `init()` fonksiyonu:

  * **`console.log(THREE.REVISION);`**: Hangi Three.js sürümünü kullandığınızı görmek için iyi bir uygulamadır.
  * **Kapsayıcı (Container) Kurulumu**: `document.createElement('div'); document.body.appendChild(container);` Bu, Three.js tuvalini tutmak için bir `div` öğesi oluşturur ve bunu HTML gövdesine ekler.
  * **Kamera Kurulumu**:
      * `new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);`
          * `75`: Derece cinsinden görüş alanı (FOV). Daha yüksek bir değer, sahnede daha fazlasını görmeniz anlamına gelir ancak daha fazla bozulma ile.
          * `window.innerWidth / window.innerHeight`: En boy oranı. Bozulmuş işlemeyi önlemek için kameranızın en boy oranını işleyicinizin/tuvalinizin en boy oranıyla eşleştirmek çok önemlidir.
          * `0.1` (yakın kırpma düzlemi): Bundan daha yakın nesneler işlenmez.
          * `1000` (uzak kırpma düzlemi): Bundan daha uzak nesneler işlenmez.
      * `camera.position.set(0, 0, 5);`: Kamerayı Z ekseni boyunca 5 birim (ekrandan dışarı) yerleştirir.
  * **Işık Kurulumu**:
      * `0x7c7c7c` (ortam) ve `0xFFFFFF` (yönlü) için değerler onaltılık renk kodlarıdır. `2.0` yoğunluktur.
      * `light.position.set( 0.32, 0.39, 0.7 );` yönlü ışığın konumunu ayarlar.
  * **İşleyici Kurulumu**:
      * `renderer.setPixelRatio( window.devicePixelRatio );`: Yüksek DPI (Retina) ekranlarda net görüntüleme sağlamaya yardımcı olur.
      * `renderer.setSize( canvasWidth, canvasHeight );`: Tuvalin boyutunu tüm pencereyi dolduracak şekilde ayarlar.
  * **Işın Dökümü ve Etkileşim ('R' tuşu kısmı):**
      * `raycaster.setFromCamera( pointer, camera );`: Bu, ışın dökümünün çekirdeğidir. `pointer`, fare konumunu temsil eden normalize edilmiş bir cihaz koordinatı (NDC) vektörüdür (x ve y için -1 ila 1). Bu satır, kameradan fare konumundan geçen bir ışın atar.
      * `const intersects = raycaster.intersectObjects( scene.children );`: Bu, sahnenizdeki hangi nesnelerin (özellikle sahnenin tüm çocukları) ışınla kesiştiğini kontrol eder.
      * **Küre Boyutu\!** `var sphereGeometry = new THREE.SphereGeometry(10, 10, 10);` 10 birim yarıçaplı bir küre oluşturur. Kameranın başlangıç konumu ve yüklü STL'nin ölçeği göz önüne alındığında, bu küre modelinize göre çok büyük veya çok küçük görünebilir. Modelinizin yanında görünür ve uygun boyutta olması için yarıçapını ayarlamanız (örn. `0.1` veya `0.05`) gerekebilir.
      * **Ok Yardımcısı Uzunluğu\!** `const length = 1;` olan `ArrowHelper` için, sahnenizin ölçeği ve küre göz önüne alındığında kolayca görülemeyecek kadar küçük olabilir. Daha belirgin hale getirmek için onu artırmayı düşünün (örn. `5` veya `10`). Ok, kesişim noktasından kameraya *doğru* olan yönü gösterir.
      * `render();` keydown dinleyicisinin içinde: Bu önemlidir. Sahneyi değiştirirseniz (bir küre veya ok eklemek gibi), görüntülenenleri güncellemek için `render()`'ı tekrar çağırmanız gerekir.
      * `window.addEventListener( 'pointermove', onPointerMove );`: Fare her hareket ettiğinde `pointer` değişkenini güncel fare koordinatlarıyla günceller.
  * **`OrbitControls` Kurulumu**:
      * `cameraControls = new THREE.OrbitControls( camera, renderer.domElement );`: Kontrolleri kameranıza ve tuvale bağlar.
      * `cameraControls.addEventListener( 'change', render );`: Bu çok önemlidir\! `OrbitControls` kameranın konumunu ve yönünü değiştirir. Kontroller bir değişikliğe neden olduğunda, güncellenmiş görünümü göstermek için sahneyi yeniden işlemeniz gerekir.
      * **Gotça: `cameraControls.update()`\!** `cameraControls.update();` genellikle kameranın `position` veya `target`'ını programlı olarak değiştirdiğinizde (daha sonra `createNewTorus`'ta görüldüğü gibi) kontrollerin yeni durumdan haberdar olmasını sağlamak için gereklidir.
  * **Sahne Arka Planı**: `scene.background = new THREE.Color( 0xAAAAAA );` sahnenizin arka plan rengini açık griye ayarlar.
  * **`createNewTorus()` çağrısı**: Bu fonksiyon, 3D modeli yüklemek ve sahneye eklemek için çağrılır.

#### `render()` fonksiyonu:

  * Bu, `renderer`'a `scene`'i `camera`'nın bakış açısından çizmesini söyleyen basit bir fonksiyondur. Görüntülenen kareyi güncellemek istediğinizde bunu çağırırsınız.

#### `createNewTorus()` fonksiyonu:

  * **STL Yükleme**:
      * `loader.load( '/basic1/Prism_hexagon.stl', function ( geometry ) { ... });`: Bu, STL dosyasını web sunucunuzun kök dizinine göre `/basic1/` dizininden yüklemeye çalışır. **Gotça: Dosya Yolu\!** `Prism_hexagon.stl` dosyasının HTML dosyanızın sunulduğu yere göre gerçekten `/basic1/Prism_hexagon.stl` konumunda olduğundan emin olun. HTML dosyasını doğrudan dosya sisteminizden açıyorsanız (örn. `file:///C:/myproject/index.html`), bu yol tarayıcı güvenlik kısıtlamaları nedeniyle çalışmaz. Dosyalarınızı sunmak için yerel bir web sunucusuna (VS Code'daki Live Server, Python'ın `http.server` veya Node.js'nin `serve` gibi) ihtiyacınız olacaktır.
  * **Geometriyi Ortalama ve Ölçeklendirme**: Bu bölüm, yüklü modeller için çok önemlidir:
      * `geometry.computeBoundingBox();`: Geometrinin tüm köşelerini içeren en küçük kutuyu hesaplar.
      * `boundingBox.getCenter(center);`: O sınırlayıcı kutunun orta noktasını bulur.
      * `geometry.translate(-center.x, -center.y, -center.z);`: Bu, geometriyi, yerel koordinat sisteminin orijininde (0,0,0) olacak şekilde hareket ettirir. Bu, nesneyi konumlandırmayı ve döndürmeyi çok daha kolay hale getirir.
      * `maxDimension` ve `scaleFactor`: Bu, yüklü modeli bir `targetSize`'a (burada 2 birim) yeniden boyutlandırmak için bir ölçek faktörü hesaplar. Bu, yüklü modelinizin sahnenizde göründüğünde çok büyük veya çok küçük olmamasını sağlamak için mükemmeldir.
      * `mesh.scale.set(scaleFactor, scaleFactor, scaleFactor);`: Hesaplanan ölçeği örgüye uygular.
  * **Malzeme**: `new THREE.MeshPhongMaterial({ color: 0xff9c7c, specular: 0x494949, shininess: 200 });` prizma için parlak bir malzeme oluşturur.
  * **Örgüyü Konumlandırma**:
      * `mesh.position.set(camera.position.x, camera.position.y, camera.position.z - distanceInFrontOfCamera);` Bu, yüklü modeli kameranın hemen önüne yerleştirir. Bu, kullanıcının modeli yüklendikten hemen sonra görebilmesini sağlamanın iyi bir yoludur. `distanceInFrontOfCamera` ne kadar önde görüneceğini kontrol eder.
  * **Gölgeler**: `mesh.castShadow = true;` ve `mesh.receiveShadow = true;` örgünün gölge atmasını ve gölge almasını sağlar. **Gotça: İşleyici Gölgeleri\!** Gölgelerin çalışması için, işleyicide de gölgeleri etkinleştirmeniz gerekir: `renderer.shadowMap.enabled = true;` (bunu `init` fonksiyonunuza ekleyin). Ayrıca, ışıklarınızın gölge atacak şekilde yapılandırılması gerekir (örn. `light.castShadow = true;`).
  * **`cameraControls.target.copy(mesh.position);`**: Bu, `OrbitControls`'ün hedefini yeni yüklenen modelinizin merkezine ayarlar. Bu, kamerayı döndürdüğünüzde, modelin etrafında döneceği anlamına gelir ki bu genellikle istediğiniz şeydir.
  * `cameraControls.update();`: Daha önce belirtildiği gibi, hedefi programlı olarak değiştirdikten sonra bunu çağırın.
  * `render();` tekrar: Model yüklendikten ve konumlandırıldıktan sonra sahneyi işler.






