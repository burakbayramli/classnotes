import org.mapsforge.core.graphics.GraphicFactory;
import org.mapsforge.core.graphics.TileBitmap;
import org.mapsforge.core.model.Tile;
import org.mapsforge.core.util.MercatorProjection;
import org.mapsforge.map.awt.graphics.AwtGraphicFactory;
import org.mapsforge.map.datastore.MapDataStore;
import org.mapsforge.map.layer.cache.FileSystemTileCache;
import org.mapsforge.map.layer.labels.TileBasedLabelStore;
import org.mapsforge.map.layer.renderer.DatabaseRenderer;
import org.mapsforge.map.layer.renderer.RendererJob;
import org.mapsforge.map.model.DisplayModel;
import org.mapsforge.map.model.FixedTileSizeDisplayModel;
import org.mapsforge.map.reader.MapFile;
import org.mapsforge.map.rendertheme.InternalRenderTheme;
import org.mapsforge.map.rendertheme.XmlRenderTheme;
import org.mapsforge.map.rendertheme.rule.RenderThemeFuture;

import java.io.File;
import java.io.IOException;

/**
 * This sample demo how to render & save a tile.
 */
public class SaveTiles {

    private static final String SAVE_PATH = "/tmp/";

    // Your compiled map. 
    private static final File DEFAULT_MAP_PATH = new File("/home/burak/Downloads/turkey.map");

    // Location you'd like to render.
    //private static final double LAT = 42.490369;
    //private static final double LAT = 42.470369;
    //private static final double LNG = 20.838044;
    private static final double LAT = 40.970041;
    private static final double LNG = 29.070311;

    private static final byte ZOOM = 14;

    public static void main(String[] args) throws IOException {
        // TODO Use args for all parameters

        // Load map.
        MapDataStore mapData = new MapFile(DEFAULT_MAP_PATH);

        // Assign tile.
        final int ty = MercatorProjection.latitudeToTileY(LAT, ZOOM);
        final int tx = MercatorProjection.longitudeToTileX(LNG, ZOOM);
        Tile tile = new Tile(tx, ty, ZOOM, 800);

        // Create requirements.
        GraphicFactory gf = AwtGraphicFactory.INSTANCE;
        XmlRenderTheme theme = InternalRenderTheme.OSMARENDER;
        DisplayModel dm = new FixedTileSizeDisplayModel(256);
        RenderThemeFuture rtf = new RenderThemeFuture(gf, theme, dm);
        RendererJob theJob = new RendererJob(tile, mapData, rtf, dm, 1.0f, false, false);
        File cacheDir = new File("/tmp", "");
        FileSystemTileCache tileCache = new FileSystemTileCache(10, cacheDir, gf, false);
        TileBasedLabelStore tileBasedLabelStore = new TileBasedLabelStore(tileCache.getCapacityFirstLevel());

        // Create renderer.
        DatabaseRenderer renderer = new DatabaseRenderer(mapData, gf, tileCache, tileBasedLabelStore, true, true, null);

        // Create RendererTheme.
        Thread t = new Thread(rtf);
        t.start();

        // Draw tile and save as PNG.
        TileBitmap tb = renderer.executeJob(theJob);
        tileCache.put(theJob, tb);

        // Close map.
        mapData.close();

        System.out.printf("Tile has been saved at %s/%d/%d/%d.tile.\n", cacheDir.getPath(), ZOOM, tx, ty);
    }
}
