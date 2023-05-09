/*
 * SVG Salamander
 * Copyright (c) 2004, Mark McKay
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or 
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 *   - Redistributions of source code must retain the above 
 *     copyright notice, this list of conditions and the following
 *     disclaimer.
 *   - Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials 
 *     provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE. 
 * 
 * Mark McKay can be contacted at mark@kitfox.com.  Salamander and other
 * projects can be found at http://www.kitfox.com
 *
 * Created on January 22, 2005, 10:30 AM
 */

package com.kitfox.svg.app.ant;

import java.awt.*;
import java.awt.image.*;
import java.util.*;
import java.util.regex.*;
import java.io.*;
import javax.imageio.*;

//import com.kitfox.util.*;
//import com.kitfox.util.indexedObject.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;

import com.kitfox.svg.app.beans.*;
import com.kitfox.svg.*;
import com.kitfox.svg.xml.ColorTable;

/**
 * <p>Translates a group of SVG files into images.</p>
 * 
 * <p>Parameters:</p>
 * <ul>
 * <li>destDir - If present, specifies a directory to write SVG files to.  Otherwise
 * writes images to directory SVG file was found in
 * verbose - If true, prints processing information to the console</li>
 * <li>format - File format for output images.  The java core javax.imageio.ImageIO
 * class is used for creating images, so format strings will depend on what
 * files your system is configured to handle.  By default, "gif", "jpg" and "png"
 * files are guaranteed to be present.  If omitted, "png" is used by default.</li>
 * <li>backgroundColor - Optional background color.  Color can be specified as a standard 
 * HTML color.  That is, as the name of a standard color such as "blue" or 
 * "limegreen", using the # notation as in #ff00ff for magenta, or in rgb format
 * listing the components as in rgb(255, 192, 192) for pink.  If omitted,
 * background is transparent.</li>
 * <li>antiAlias - If set, shapes are drawn using anti-aliasing.  Defaults to true.</li>
 * <li>interpolation - String describing image interpolation algorithm.  Can
 * be one of "nearest neighbor", "bilinear" or "bicubic".  Defaults to "bicubic".</li>
 * <li>width - If greater than 0, determines the width of the written image.  Otherwise,
 * the width is obtained from the SVG document.  Defaults to -1;</li>
 * <li>height - If greater than 0, determines the height of the written image.  Otherwise,
 * the height is obtained from the SVG document.  Defaults to -1.</li>
 * <li>sizeToFit - If true and the width and height of the output image differ
 * from that of the SVG image, the valid area of the SVG image will be resized 
 * to fit the specified size.</li>
 * <li>verbose - If true, prints out diagnostic information about processing.  
 * Defaults to false.</li>
 * </ul>
 * 
 * Example:
 * &lt;SVGToImage destDir="${index.java}" format="jpg" verbose="true"&gt;
 *    &lt;fileset dir="${dir1}"&gt;
 *        &lt;include name="*.svg"/&gt;
 *    &lt;/fileset&gt;
 *    &lt;fileset dir="${dir2}"&gt;
 *        &lt;include name="*.svg"/&gt;
 *    &lt;/fileset&gt;
 * &lt;/SVGToImage&gt;
 * 
 * 
 * 
 * @author kitfox
 */
public class SVGToImageAntTask extends Task
{
    private ArrayList<FileSet> filesets = new ArrayList<FileSet>();
    boolean verbose = false;
    File destDir;
    private String format = "png";
    Color backgroundColor = null;
    int width = -1;
    int height = -1;
    boolean antiAlias = true;
    String interpolation = "bicubic";
    boolean clipToViewBox = false;
    boolean sizeToFit = true;
    
    /** Creates a new instance of IndexLoadObjectsAntTask */
    public SVGToImageAntTask()
    {
    }
    
    
    public String getFormat()
    {
        return format;
    }
    
    public void setFormat(String format)
    {
        this.format = format;
    }
    
    public void setBackgroundColor(String bgColor)
    {
        this.backgroundColor = ColorTable.parseColor(bgColor);
    }
    
    public void setHeight(int height)
    {
        this.height = height;
    }
    
    public void setWidth(int width)
    {
        this.width = width;
    }
    
    public void setAntiAlias(boolean antiAlias)
    {
        this.antiAlias = antiAlias;
    }
    
    public void setInterpolation(String interpolation)
    {
        this.interpolation = interpolation;
    }
    
    public void setSizeToFit(boolean sizeToFit)
    {
        this.sizeToFit = sizeToFit;
    }
    
    public void setClipToViewBox(boolean clipToViewBox)
    {
        this.clipToViewBox = clipToViewBox;
    }
    
    public void setVerbose(boolean verbose)
    {
        this.verbose = verbose;
    }
    
    public void setDestDir(File destDir)
    {
        this.destDir = destDir;
    }
    
    /**
     * Adds a set of files.
     * @param set
     */
    public void addFileset(FileSet set)
    {
        filesets.add(set);
    }
    
    
    
    @Override
    public void execute()
    {
        if (verbose) log("Building SVG images");
        
        for (FileSet fs : filesets) {
            FileScanner scanner = fs.getDirectoryScanner(getProject());
            String[] files = scanner.getIncludedFiles();
            
            try
            {
                File basedir = scanner.getBasedir();
                
                if (verbose) log("Scaning " + basedir);
                
                for (int i = 0; i < files.length; i++)
                {
//System.out.println("File " + files[i]);
//System.out.println("BaseDir " + basedir);
                    translate(basedir, files[i]);
                }
            }
            catch (Exception e)
            {
                throw new BuildException(e);
            }
        }
    }
    
    private void translate(File baseDir, String shortName) throws BuildException
    {
        File source = new File(baseDir, shortName);
        
        if (verbose) log("Reading file: " + source);
        
        Matcher matchName = Pattern.compile("(.*)\\.svg", Pattern.CASE_INSENSITIVE).matcher(shortName);
        if (matchName.matches())
        {
            shortName = matchName.group(1);
        }
        shortName += "." + format;
        
        SVGIcon icon = new SVGIcon();
        icon.setSvgURI(source.toURI());
        icon.setAntiAlias(antiAlias);
        if (interpolation.equals("nearest neighbor"))
        {
            icon.setInterpolation(SVGIcon.INTERP_NEAREST_NEIGHBOR);
        }
        else if (interpolation.equals("bilinear"))
        {
            icon.setInterpolation(SVGIcon.INTERP_BILINEAR);
        }
        else if (interpolation.equals("bicubic"))
        {
            icon.setInterpolation(SVGIcon.INTERP_BICUBIC);
        }
        
        int iconWidth = width > 0 ? width : icon.getIconWidth();
        int iconHeight = height > 0 ? height : icon.getIconHeight();
        icon.setClipToViewbox(clipToViewBox);
        icon.setPreferredSize(new Dimension(iconWidth, iconHeight));
        icon.setScaleToFit(sizeToFit);
        BufferedImage image = new BufferedImage(iconWidth, iconHeight, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = image.createGraphics();
        
        if (backgroundColor != null)
        {
            g.setColor(backgroundColor);
            g.fillRect(0, 0, iconWidth, iconHeight);
        }
        
        g.setClip(0, 0, iconWidth, iconHeight);
//        g.fillRect(10, 10, 100, 100);
        icon.paintIcon(null, g, 0, 0);
        g.dispose();
        
        File outFile = destDir == null ? new File(baseDir, shortName) : new File(destDir, shortName);
        if (verbose) log("Writing file: " + outFile);
        
        try
        {
            ImageIO.write(image, format, outFile);
        }
        catch (IOException e)
        {
            log("Error writing image: " + e.getMessage());
            throw new BuildException(e);
        }
        
        
        SVGCache.getSVGUniverse().clear();
    }
    
}
