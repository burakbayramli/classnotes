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
 * Created on January 26, 2004, 8:40 PM
 */

package com.kitfox.svg.pathcmd;

//import org.apache.batik.ext.awt.geom.ExtendedGeneralPath;
import java.awt.geom.*;

/**
 * @author Mark McKay
 * @author <a href="mailto:mark@kitfox.com">Mark McKay</a>
 */
public class Vertical extends PathCommand {

    public float y = 0f;

    /** Creates a new instance of MoveTo */
    public Vertical() {
    }

    @Override
    public String toString()
    {
        return "V " + y;
    }

    public Vertical(boolean isRelative, float y) {
        super(isRelative);
        this.y = y;
    }

//    public void appendPath(ExtendedGeneralPath path, BuildHistory hist)
    @Override
    public void appendPath(GeneralPath path, BuildHistory hist)
    {
        float offx = hist.lastPoint.x;
        float offy = isRelative ? hist.lastPoint.y : 0f;

        path.lineTo(offx, y + offy);
        hist.setLastPoint(offx, y + offy);
        hist.setLastKnot(offx, y + offy);
    }

    @Override
    public int getNumKnotsAdded()
    {
        return 2;
    }
}
