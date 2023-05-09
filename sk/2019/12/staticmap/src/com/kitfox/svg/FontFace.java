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
 * Created on February 20, 2004, 10:00 PM
 */
package com.kitfox.svg;

import com.kitfox.svg.xml.StyleAttribute;

/**
 * Implements an embedded font.
 *
 * SVG specification: http://www.w3.org/TR/SVG/fonts.html
 *
 * @author Mark McKay
 * @author <a href="mailto:mark@kitfox.com">Mark McKay</a>
 */
public class FontFace extends SVGElement
{

    public static final String TAG_NAME = "fontface";
    String fontFamily;
    /**
     * Em size of coordinate system font is defined in
     */
    private int unitsPerEm = 1000;
    private int ascent = -1;
    private int descent = -1;
    private int accentHeight = -1;
    private int underlinePosition = -1;
    private int underlineThickness = -1;
    private int strikethroughPosition = -1;
    private int strikethroughThickness = -1;
    private int overlinePosition = -1;
    private int overlineThickness = -1;

    /**
     * Creates a new instance of Font
     */
    public FontFace()
    {
    }

    @Override
    public String getTagName()
    {
        return TAG_NAME;
    }

    @Override
    protected void build() throws SVGException
    {
        super.build();

        StyleAttribute sty = new StyleAttribute();

        if (getPres(sty.setName("font-family")))
        {
            fontFamily = sty.getStringValue();
        }

        if (getPres(sty.setName("units-per-em")))
        {
            unitsPerEm = sty.getIntValue();
        }
        if (getPres(sty.setName("ascent")))
        {
            ascent = sty.getIntValue();
        }
        if (getPres(sty.setName("descent")))
        {
            descent = sty.getIntValue();
        }
        if (getPres(sty.setName("accent-height")))
        {
            accentHeight = sty.getIntValue();
        }

        if (getPres(sty.setName("underline-position")))
        {
            underlinePosition = sty.getIntValue();
        }
        if (getPres(sty.setName("underline-thickness")))
        {
            underlineThickness = sty.getIntValue();
        }
        if (getPres(sty.setName("strikethrough-position")))
        {
            strikethroughPosition = sty.getIntValue();
        }
        if (getPres(sty.setName("strikethrough-thickenss")))
        {
            strikethroughThickness = sty.getIntValue();
        }
        if (getPres(sty.setName("overline-position")))
        {
            overlinePosition = sty.getIntValue();
        }
        if (getPres(sty.setName("overline-thickness")))
        {
            overlineThickness = sty.getIntValue();
        }
    }

    public String getFontFamily()
    {
        return fontFamily;
    }

    public int getUnitsPerEm()
    {
        return unitsPerEm;
    }

    public int getAscent()
    {
        if (ascent == -1)
        {
            ascent = unitsPerEm - ((Font) parent).getVertOriginY();
        }
        return ascent;
    }

    public int getDescent()
    {
        if (descent == -1)
        {
            descent = ((Font) parent).getVertOriginY();
        }
        return descent;
    }

    public int getAccentHeight()
    {
        if (accentHeight == -1)
        {
            accentHeight = getAscent();
        }
        return accentHeight;
    }

    public int getUnderlinePosition()
    {
        if (underlinePosition == -1)
        {
            underlinePosition = unitsPerEm * 5 / 6;
        }
        return underlinePosition;
    }

    public int getUnderlineThickness()
    {
        if (underlineThickness == -1)
        {
            underlineThickness = unitsPerEm / 20;
        }
        return underlineThickness;
    }

    public int getStrikethroughPosition()
    {
        if (strikethroughPosition == -1)
        {
            strikethroughPosition = unitsPerEm * 3 / 6;
        }
        return strikethroughPosition;
    }

    public int getStrikethroughThickness()
    {
        if (strikethroughThickness == -1)
        {
            strikethroughThickness = unitsPerEm / 20;
        }
        return strikethroughThickness;
    }

    public int getOverlinePosition()
    {
        if (overlinePosition == -1)
        {
            overlinePosition = unitsPerEm * 5 / 6;
        }
        return overlinePosition;
    }

    public int getOverlineThickness()
    {
        if (overlineThickness == -1)
        {
            overlineThickness = unitsPerEm / 20;
        }
        return overlineThickness;
    }

    /**
     * Updates all attributes in this diagram associated with a time event. Ie,
     * all attributes with track information.
     *
     * @return - true if this node has changed state as a result of the time
     * update
     */
    @Override
    public boolean updateTime(double curTime)
    {
        //Fonts can't change
        return false;
    }

    /**
     * @param unitsPerEm the unitsPerEm to set
     */
    public void setUnitsPerEm(int unitsPerEm)
    {
        this.unitsPerEm = unitsPerEm;
    }

    /**
     * @param ascent the ascent to set
     */
    public void setAscent(int ascent)
    {
        this.ascent = ascent;
    }

    /**
     * @param descent the descent to set
     */
    public void setDescent(int descent)
    {
        this.descent = descent;
    }

    /**
     * @param accentHeight the accentHeight to set
     */
    public void setAccentHeight(int accentHeight)
    {
        this.accentHeight = accentHeight;
    }

    /**
     * @param underlinePosition the underlinePosition to set
     */
    public void setUnderlinePosition(int underlinePosition)
    {
        this.underlinePosition = underlinePosition;
    }

    /**
     * @param underlineThickness the underlineThickness to set
     */
    public void setUnderlineThickness(int underlineThickness)
    {
        this.underlineThickness = underlineThickness;
    }

    /**
     * @param strikethroughPosition the strikethroughPosition to set
     */
    public void setStrikethroughPosition(int strikethroughPosition)
    {
        this.strikethroughPosition = strikethroughPosition;
    }

    /**
     * @param strikethroughThickness the strikethroughThickness to set
     */
    public void setStrikethroughThickness(int strikethroughThickness)
    {
        this.strikethroughThickness = strikethroughThickness;
    }

    /**
     * @param overlinePosition the overlinePosition to set
     */
    public void setOverlinePosition(int overlinePosition)
    {
        this.overlinePosition = overlinePosition;
    }

    /**
     * @param overlineThickness the overlineThickness to set
     */
    public void setOverlineThickness(int overlineThickness)
    {
        this.overlineThickness = overlineThickness;
    }
}
