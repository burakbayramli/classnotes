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
 */

package com.kitfox.svg.app.data;

import com.kitfox.svg.SVGConst;
import com.kitfox.svg.util.Base64InputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author kitfox
 */
public class Handler extends URLStreamHandler
{
    class Connection extends URLConnection
    {
        String mime;
        byte[] buf;

        public Connection(URL url)
        {
            super(url);

            String path = url.getPath();
            int idx = path.indexOf(';');
            mime = path.substring(0, idx);
            String content = path.substring(idx + 1);

            if (content.startsWith("base64,"))
            {
                content = content.substring(7);
                try
                {
//byte[] buf2 = new sun.misc.BASE64Decoder().decodeBuffer(content);
//buf = new sun.misc.BASE64Decoder().decodeBuffer(content);
                    
                    ByteArrayInputStream bis = new ByteArrayInputStream(content.getBytes());
                    Base64InputStream b64is = new Base64InputStream(bis);
                    
                    ByteArrayOutputStream bout = new ByteArrayOutputStream();
                    byte[] tmp = new byte[2056];
                    for (int size = b64is.read(tmp); size != -1; size = b64is.read(tmp))
                    {
                        bout.write(tmp, 0, size);
                    }
                    buf = bout.toByteArray();
                }
                catch (IOException e)
                {
                    Logger.getLogger(SVGConst.SVG_LOGGER).log(Level.WARNING, null, e);
                }
            }
        }
        
        @Override
        public void connect() throws IOException
        {
        }

        @Override
        public String getHeaderField(String name)
        {
            if ("content-type".equals(name))
            {
                return mime;
            }

            return super.getHeaderField(name);
        }

        @Override
        public InputStream getInputStream() throws IOException
        {
            return new ByteArrayInputStream(buf);
        }

//        public Object getContent() throws IOException
//        {
//            BufferedImage img = ImageIO.read(getInputStream());
//        }
    }

    @Override
    protected URLConnection openConnection(URL u) throws IOException
    {
        return new Connection(u);
    }

}
