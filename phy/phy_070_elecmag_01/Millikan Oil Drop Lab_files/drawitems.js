function LabQuest(u){

	/* Calculates the time to go on the LabQuest display */
	
	d = new Date();
	
	h = d.getHours();
	m = d.getMinutes();
	
	z = "AM";
	
	if (h == 0){
		h = 12;
	}
	if (h > 12){
		h = h - 12;
		z = "PM";
	}
	
	if (h < 10){
		h = "0" + h;
	}
	
	if (m < 10){
		m = "0" + m;
	}
	
	displaytime = h + ":" + m  + z;
	
	/*  Generates a small random error to make display reading flicker */
	
	ForceDisplay = Force;
	
	if (u != 1){
		ED = Math.random()*2;
		if (ED > 1){
			ErrorDirection = 1;
		}
		else{
			ErrorDirection = -1;
		}
		
		if (Force > 1){
			Error = Math.random()*Force*0.002*ErrorDirection;
		}
		else{
			Error = Math.random()*.01*ErrorDirection;
		}
		
		ForceDisplay = Force+Error;
		
		/*  Makes sure the displayed force is not larger than Maximum Value */
		
		if (ForceDisplay > 50){
			ForceDisplay = 50;
		}
	}
	
	
	
	/*  Draws the body of the Lab Quest */
	
	ctx2.lineWidth = 1;
	ctx2.strokeStyle = "#444444";
	ctx2.fillStyle = "#444444";
	ctx2.beginPath();
	ctx2.moveTo(LabQuestX-LabQuestHalfWidth+LabQuestCurve,LabQuestY);
	ctx2.arc(LabQuestX-LabQuestHalfWidth+LabQuestCurve, LabQuestY-LabQuestCurve, LabQuestCurve, 0.5*Math.PI, 1*Math.PI);
	ctx2.lineTo(LabQuestX-LabQuestHalfWidth,LabQuestY-LabQuestHeight+LabQuestCurve);
	ctx2.arc(LabQuestX-LabQuestHalfWidth+LabQuestCurve, LabQuestY-LabQuestHeight+LabQuestCurve, LabQuestCurve, 1*Math.PI, 1.5*Math.PI);
	ctx2.lineTo(LabQuestX+LabQuestHalfWidth-LabQuestCurve,LabQuestY-LabQuestHeight);
	ctx2.arc(LabQuestX+LabQuestHalfWidth-LabQuestCurve, LabQuestY-LabQuestHeight+LabQuestCurve, LabQuestCurve, 1.5*Math.PI, 0*Math.PI);
	ctx2.lineTo(LabQuestX+LabQuestHalfWidth,LabQuestY-LabQuestCurve);
	ctx2.arc(LabQuestX+LabQuestHalfWidth-LabQuestCurve, LabQuestY-LabQuestCurve, LabQuestCurve, 0*Math.PI, 0.5*Math.PI);
	ctx2.lineTo(LabQuestX-LabQuestHalfWidth+LabQuestCurve,LabQuestY);
	ctx2.closePath();
	ctx2.fill();
	ctx2.stroke();
	
	/*  Rectangles on Screen */
	
	ctx2.fillStyle = "#49999e";
	ctx2.fillRect(LabQuestX-0.75*LabQuestHalfWidth,LabQuestY-0.825*LabQuestHeight,1.5*LabQuestHalfWidth,0.75*LabQuestHeight);
		
	ctx2.fillStyle = "#397b7d";
	ctx2.strokeStyle = "#ffffff";
	ctx2.fillRect(LabQuestX-0.7425*LabQuestHalfWidth,LabQuestY-0.75*LabQuestHeight,1.01*LabQuestHalfWidth,0.625*LabQuestHeight);
	ctx2.strokeRect(LabQuestX-0.7425*LabQuestHalfWidth,LabQuestY-0.75*LabQuestHeight,1.01*LabQuestHalfWidth,0.625*LabQuestHeight);
	
	ctx2.fillStyle = "#ea212c";
	ctx2.strokeStyle = "#ffffff";
	ctx2.fillRect(LabQuestX-0.7375*LabQuestHalfWidth,LabQuestY-0.6875*LabQuestHeight,1.0*LabQuestHalfWidth,0.5*LabQuestHeight);
	ctx2.strokeRect(LabQuestX-0.7375*LabQuestHalfWidth,LabQuestY-0.6875*LabQuestHeight,1.0*LabQuestHalfWidth,0.5*LabQuestHeight);
	
	ctx2.fillStyle = "#555555";
	ctx2.fillRect(LabQuestX-0.75*LabQuestHalfWidth,LabQuestY-0.125*LabQuestHeight,1.5*LabQuestHalfWidth,0.065*LabQuestHeight);
	
	ctx2.fillStyle = "#FFFFFF";
	ctx2.strokeStyle = "#000000";
	ctx2.fillRect(LabQuestX-0.725*LabQuestHalfWidth,LabQuestY-0.82*LabQuestHeight,0.065*LabQuestHeight,0.065*LabQuestHeight);
	ctx2.strokeRect(LabQuestX-0.725*LabQuestHalfWidth,LabQuestY-0.82*LabQuestHeight,0.065*LabQuestHeight,0.065*LabQuestHeight);
	
	ctx2.fillStyle = "#c2d6d8";
	ctx2.fillRect(LabQuestX+0.2875*LabQuestHalfWidth,LabQuestY-0.75*LabQuestHeight,0.45*LabQuestHalfWidth,0.35*LabQuestHeight);
	
	ctx2.strokeStyle = "#49999e";
	ctx2.strokeRect(LabQuestX+0.3*LabQuestHalfWidth,LabQuestY-0.7375*LabQuestHeight,0.425*LabQuestHalfWidth,0.1*LabQuestHeight);
	ctx2.strokeRect(LabQuestX+0.3*LabQuestHalfWidth,LabQuestY-0.625*LabQuestHeight,0.425*LabQuestHalfWidth,0.1*LabQuestHeight);
	ctx2.strokeRect(LabQuestX+0.3*LabQuestHalfWidth,LabQuestY-0.5125*LabQuestHeight,0.425*LabQuestHalfWidth,0.1*LabQuestHeight);
	
	/*  Words on Lab Quest */
	
	/*  Vernier */
	
	ctx2.font = 0.1*LabQuestHeight + "px Arial";
	ctx2.fillStyle="#FFFFFF";
	temptext = "Vernier";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	ctx2.save();
	ctx2.translate(CanvasCenterX,CanvasCenterY);
	ctx2.rotate(-0.5*Math.PI);
	ctx2.fillText(temptext,-0.5*textWidth,-0.83*LabQuestHalfWidth);
	ctx2.restore();


	
	/* Lab Quest */
	
	ctx2.font= 0.1*LabQuestHeight + "px Arial";
	ctx2.fillStyle="#49999e";
	temptext = "LabQuest ®";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX-0.375*LabQuestHalfWidth-0.5*textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.875*LabQuestHeight);
	
	ctx2.fillStyle="#ea212c";
	temptext = "2";
	xposition = LabQuestX-0.3625*LabQuestHalfWidth+0.5*textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.875*LabQuestHeight);
	
	
	/* Connected Science System */
	ctx2.font = 0.05*LabQuestHeight + "px Arial";
	ctx2.fillStyle="#FFFFFF";
	temptext = "CONNECTED SCIENCE SYSTEM";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.35*LabQuestHalfWidth-0.5*textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.0125*LabQuestHeight);
	
	
	/* Force Channel 1 */
	ctx2.font = 0.075*LabQuestHeight + "px Arial bold";
	ctx2.fillStyle="#FFFFFF";
	temptext = "CH 1: FORCE";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX-0.475*LabQuestHalfWidth-0.5*textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.575*LabQuestHeight);
	

	/* Force Reading */
	ctx2.font = 0.15*LabQuestHeight + "px Arial bold";
	ctx2.fillStyle="#FFFFFF";
	if (u != 1){
		temptext = ForceDisplay.toFixed(2) + " N";
	}
	else{
		temptext = ForceDisplay;
	}
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.125*LabQuestHalfWidth-textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.375*LabQuestHeight);
	
	/* Display Time */
	ctx2.font = 0.035*LabQuestHeight + "px Arial";
	ctx2.fillStyle="#FFFFFF";
	temptext = displaytime;
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.725*LabQuestHalfWidth-textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.0875*LabQuestHeight);
	
	/* Menus */
	ctx2.font = 0.035*LabQuestHeight + "px Arial";
	ctx2.fillStyle="#FFFFFF";
	temptext = "File    Sensors";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX-0.625*LabQuestHalfWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.775*LabQuestHeight);
	
	/* Data Collection Options */
	ctx2.font = 0.035*LabQuestHeight + "px Arial Bold";
	ctx2.fillStyle="#000000";
	temptext = "Mode:";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.3125*LabQuestHalfWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.7*LabQuestHeight);
	
	temptext = "Rate:";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.3125*LabQuestHalfWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.5875*LabQuestHeight);
	
	temptext = "Length:";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.3125*LabQuestHalfWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.475*LabQuestHeight);
	
	ctx2.font = 0.035*LabQuestHeight + "px Arial";
	ctx2.fillStyle="#000000";
	temptext = "Time Based";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.5125*LabQuestHalfWidth - 0.5*textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.6625*LabQuestHeight);
	
	temptext = "1.0 samples/s";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.5125*LabQuestHalfWidth - 0.5*textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.55*LabQuestHeight);
	
	temptext = "180.0 s";
	metrics = ctx2.measureText(temptext);
	textWidth = metrics.width;
	xposition = LabQuestX+0.5125*LabQuestHalfWidth - 0.5*textWidth;
	ctx2.fillText(temptext,xposition, LabQuestY-0.4375*LabQuestHeight);
	
}

function Cylinder(ccx,cty,chw,ch,cc,cc1,cc2,lw){
	
/* ccx:  cylinder center x cty:  cylinder top y  chw:  cylinder halfwidth ch: cylinder height  cc:  cylinder curve  cc1:  cylinder top color  cc2:  cylinder color  lw:  linewidth */
	
	ctx.lineWidth = lw;
	ctx.strokeStyle = "#000000";
	ctx.beginPath();
	ctx.moveTo(ccx-chw, cty);
	ctx.bezierCurveTo(ccx-chw,cty+cc,ccx+chw,cty+cc,ccx+chw,cty);
	ctx.lineTo(ccx+chw,cty+ch);
	ctx.bezierCurveTo(ccx+chw,cty+ch+cc,ccx-chw,cty+ch+cc,ccx-chw,cty+ch);
	ctx.lineTo(ccx-chw, cty);
	ctx.stroke();
	ctx.closePath();
	ctx.fillStyle = cc2;
	ctx.fill();
	
	ctx.lineWidth = lw;
	ctx.strokeStyle = "#000000";
	ctx.beginPath();
	ctx.moveTo(ccx-chw, cty);
	ctx.bezierCurveTo(ccx-chw,cty-cc,ccx+chw,cty-cc,ccx+chw,cty);
	ctx.moveTo(ccx-chw, cty);
	ctx.bezierCurveTo(ccx-chw,cty+cc,ccx+chw,cty+cc,ccx+chw,cty);
	ctx.stroke();
	ctx.closePath();
	ctx.fillStyle = cc1;
	ctx.fill();
	
	
}

function BackOfCylinder(ccx,cty,chw,ch,cc,cc1,cc2,lw){
	
	ctx.lineWidth = lw;
	ctx.strokeStyle = "#000000";
	ctx.beginPath();
	ctx.moveTo(ccx-chw, cty);
	ctx.bezierCurveTo(ccx-chw,cty-cc,ccx+chw,cty-cc,ccx+chw,cty);
	ctx.lineTo(ccx+chw,cty+ch);
	ctx.bezierCurveTo(ccx+chw,cty+ch-cc,ccx-chw,cty+ch-cc,ccx-chw,cty+ch);
	ctx.lineTo(ccx-chw, cty);
	ctx.stroke();
	ctx.closePath();
	ctx.fillStyle = cc2;
	ctx.fill();
		
}

function FrontOfCylinder(ccx,cty,chw,ch,cc,cc1,cc2,lw){
	
	ctx.lineWidth = lw;
	ctx.strokeStyle = "#000000";
	ctx.beginPath();
	ctx.moveTo(ccx-chw, cty);
	ctx.bezierCurveTo(ccx-chw,cty+cc,ccx+chw,cty+cc,ccx+chw,cty);
	ctx.lineTo(ccx+chw,cty+ch);
	ctx.bezierCurveTo(ccx+chw,cty+ch+cc,ccx-chw,cty+ch+cc,ccx-chw,cty+ch);
	ctx.lineTo(ccx-chw, cty);
	ctx.stroke();
	ctx.closePath();
	ctx.fillStyle = cc1;
	ctx.fill();
		
}

function Photogate(pcx, pty, phw, ph){

	/* support metal */
	ctx.lineWidth = 8;
	ctx.strokeStyle = "#C0C0C0";
	
	ctx.beginPath();
	ctx.moveTo(pcx-phw, pty+0.4*ph);
	ctx.lineTo(-10, pty+0.4*ph);
	ctx.stroke();
	ctx.closePath();
	

	ctx.lineWidth = 1;
	ctx.strokeStyle = "#999999";
	ctx.fillStyle = "#333333";
	
	/* Left Side */
	ctx.beginPath();
	
	ctx.moveTo(pcx-phw+2.0*ph, pty-2.0*ph);
	
	ctx.lineTo(pcx-phw+2.0*ph, pty-1.0*ph);
	
	ctx.lineTo(pcx-phw+1.0*ph, pty+1.0*ph);
	
	ctx.lineTo(pcx-phw+1.0*ph, pty);
	
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	/* Top */
	ctx.beginPath();
	ctx.moveTo(pcx+phw, pty);
	
	ctx.lineTo(pcx+phw+1.0*ph, pty-2.0*ph);
	
	ctx.lineTo(pcx+phw, pty-2.0*ph);
	
	ctx.lineTo(pcx+phw-0.75*ph, pty-0.5*ph);
	
	ctx.lineTo(pcx-phw+1.25*ph, pty-0.5*ph);
	
	ctx.lineTo(pcx-phw+2.0*ph, pty-2.0*ph);
	
	ctx.lineTo(pcx-phw+1.0*ph, pty-2.0*ph);
	
	ctx.lineTo(pcx-phw, pty);
	ctx.lineTo(pcx-phw, pty+ph);
	ctx.lineTo(pcx+phw, pty+ph);
	ctx.lineTo(pcx+phw, pty);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	/* Front Side */
	ctx.beginPath();
	ctx.moveTo(pcx, pty);
	ctx.lineTo(pcx+phw, pty);
	ctx.lineTo(pcx+phw, pty+ph);
	ctx.lineTo(pcx-phw, pty+ph);
	ctx.lineTo(pcx-phw, pty);
	ctx.lineTo(pcx, pty);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	/* Right Side */
	ctx.beginPath();
	ctx.moveTo(pcx+phw, pty+ph);
	
	ctx.lineTo(pcx+phw+1.0*ph, pty-1.0*ph);
	
	ctx.lineTo(pcx+phw+1.0*ph, pty-2.0*ph);
	
	ctx.lineTo(pcx+phw, pty);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	/* line on top right side */
	ctx.beginPath();
	ctx.moveTo(pcx+phw, pty);
	ctx.lineTo(pcx+phw-0.75*ph, pty-0.5*ph);
	ctx.stroke();
	ctx.closePath();
	
	/* Text on Front */
	ctx.font="10px Arial";
	ctx.fillStyle="#FFFFFF";
	temptext = "Vernier Photogate";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = pcx + 0.2*phw - textWidth/2;
	ctx.fillText(temptext,xposition, pty+0.7*ph);
	
	/* Red Indicator Light */
	ctx.lineWidth = 1;
	ctx.strokeStyle = "#FFFFFF";
	ctx.fillStyle = "#660000";
	
	ctx.beginPath();
	ctx.arc(pcx-0.8*phw, pty+0.5*ph, 0.2*ph, 0, 2*Math.PI);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();


}

function Sky(sx,sy,ex,ey,sw,sh,ssc,esc){

	grd=ctx.createLinearGradient(sx,sy,ex,ey);
	grd.addColorStop(0,ssc);
	grd.addColorStop(1,esc);

	ctx.fillStyle=grd;
	ctx.fillRect(sx,sy,sw,sh);
	
}

function CityBuilding(bx,by,bw,bh, bc, bs){
	ctx.lineWidth = 1;
	ctx.strokeStyle = "#000000";
	ctx.fillStyle=bc;
	if (bs == 0){
		ctx.beginPath();
		ctx.moveTo(bx,by);
		ctx.lineTo(bx+bw, by+10);
		ctx.lineTo(bx+bw, by+bh);
		ctx.lineTo(bx, by+bh);
		ctx.lineTo(bx, by);
		ctx.stroke();
		ctx.closePath();
		ctx.fill();
	}
	if (bs == 1){
		ctx.beginPath();
		ctx.moveTo(bx,by);
		ctx.lineTo(bx+0.5*bw, by-10);
		ctx.lineTo(bx+bw,by);
		ctx.lineTo(bx+bw, by+bh);
		ctx.lineTo(bx, by+bh);
		ctx.lineTo(bx, by);
		ctx.stroke();
		ctx.closePath();
		ctx.fill();
	}
	if (bs == 2){
		ctx.beginPath();
		ctx.moveTo(bx,by);
		ctx.lineTo(bx+bw, by-10);
		ctx.lineTo(bx+bw, by+bh);
		ctx.lineTo(bx, by+bh);
		ctx.lineTo(bx, by);
		ctx.stroke();
		ctx.closePath();
		ctx.fill();
	}
	if (bs > 2){
		ctx.beginPath();
		ctx.moveTo(bx,by);
		ctx.lineTo(bx+bw, by);
		ctx.lineTo(bx+bw, by+bh);
		ctx.lineTo(bx, by+bh);
		ctx.lineTo(bx, by);
		ctx.stroke();
		ctx.closePath();
		ctx.fill();
	}
	
}

function Boat(fx,ty,lob,hob,db,cob, nob){
	
	if (db == 1){
		ctx.lineWidth = 1;
		ctx.strokeStyle = "#000000";
		ctx.fillStyle = "#EEEEDD";
		ctx.fillRect(fx-0.95*lob, ty-30, 50, 40);
		ctx.strokeRect(fx-0.95*lob, ty-30, 50, 40);
		ctx.lineWidth = 3;
		ctx.strokeStyle = "#EEEEDD";
		ctx.beginPath();
		ctx.moveTo(fx-.95*lob+20, ty-40);
		ctx.lineTo(fx-.95*lob+20, ty-30);
		ctx.moveTo(fx-.95*lob+5, ty-34);
		ctx.lineTo(fx-.95*lob+35, ty-34);
		ctx.stroke();
		ctx.closePath();
		ctx.lineWidth = 1;
		ctx.strokeStyle = "#000000";
		ctx.fillStyle = cob;
		ctx.beginPath();
		ctx.moveTo(fx,ty);
		ctx.lineTo(fx-0.05*lob, ty+hob);
		ctx.lineTo(fx-lob, ty+hob);
		ctx.lineTo(fx-lob, ty+0.08*hob);
		ctx.lineTo(fx-0.1*lob, ty+0.08*hob);
		ctx.lineTo(fx, ty);
		ctx.stroke();
		ctx.closePath();
		ctx.fill();
	}
	else{
		ctx.lineWidth = 1;
		ctx.strokeStyle = "#000000";
		ctx.fillStyle = "#EEEEDD";
		ctx.fillRect(fx+0.95*lob-50, ty-30, 50, 40);
		ctx.strokeRect(fx+0.95*lob-50, ty-30, 50, 40);
		ctx.lineWidth = 3;
		ctx.strokeStyle = "#EEEEDD";
		ctx.beginPath();
		ctx.moveTo(fx+.95*lob-20, ty-40);
		ctx.lineTo(fx+.95*lob-20, ty-30);
		ctx.moveTo(fx+.95*lob-5, ty-34);
		ctx.lineTo(fx+.95*lob-35, ty-34);
		ctx.stroke();
		ctx.closePath();
		ctx.lineWidth = 1;
		ctx.strokeStyle = "#000000";
		ctx.fillStyle = cob;
		ctx.beginPath();
		ctx.moveTo(fx,ty);
		ctx.lineTo(fx+0.05*lob, ty+hob);
		ctx.lineTo(fx+lob, ty+hob);
		ctx.lineTo(fx+lob, ty+0.08*hob);
		ctx.lineTo(fx+0.1*lob, ty+0.08*hob);
		ctx.lineTo(fx, ty);
		ctx.stroke();
		ctx.closePath();
		ctx.fill();
	}
	
	ctx.font="16px Arial";
	ctx.fillStyle="#FFFFFF";
	sizeofboat = 0.1*lob;
	temptext = "Boat: " + nob + " Length: " + sizeofboat.toFixed(1) + " m ";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = fx - db*0.5*lob - textWidth/2;
	ctx.fillText(temptext,xposition, ty+25);
}

function DrawLabTable(x, y, w, t){
	ctx.strokeStyle = "000000";
	ctx.fillStyle="#666666";
	ctx.lineWidth = 4;
	ctx.fillRect(x,y,w,t);
	
	ctx.fillStyle="#85bae6";
	ctx.fillRect(x,y+t,w-0.5*t,1.5*t);
	ctx.strokeRect(x,y+t,w-0.5*t,1.5*t);
	
	ctx.fillStyle="#85bae6";
	ctx.fillRect(x+0.9*w,y+2.5*t,t,910-y);
	ctx.strokeRect(x+0.9*w,y+2.5*t,t,910-y);
	
}


function CylindricalMass(ccx,cty,size,cc1,cc2){
	
/* ccx:  cylinder center x cty:  cylinder top y  chw:  cylinder halfwidth ch: cylinder height  cc:  cylinder curve  cc1:  cylinder top color  cc2:  cylinder color  lw:  linewidth */
	
	ctx.lineWidth = 0.02*size;
	ctx.strokeStyle = "#000000";
	ctx.beginPath();
	ctx.moveTo(ccx-0.5*size, cty);
	ctx.bezierCurveTo(ccx-0.5*size,cty+0.05*size,ccx+0.5*size,cty+0.05*size,ccx+0.5*size,cty);
	ctx.lineTo(ccx+0.5*size,cty+1.5*size);
	ctx.bezierCurveTo(ccx+0.5*size,cty+1.5*size+0.05*size,ccx-0.5*size,cty+1.5*size+0.05*size,ccx-0.5*size,cty+1.5*size);
	ctx.lineTo(ccx-0.5*size, cty);
	ctx.stroke();
	ctx.closePath();
	ctx.fillStyle = cc2;
	ctx.fill();
	
	ctx.lineWidth = 0.02*size;
	ctx.strokeStyle = "#000000";
	ctx.beginPath();
	ctx.moveTo(ccx-0.5*size, cty);
	ctx.bezierCurveTo(ccx-0.5*size,cty-0.05*size,ccx+0.5*size,cty-0.05*size,ccx+0.5*size,cty);
	ctx.moveTo(ccx-0.5*size, cty);
	ctx.bezierCurveTo(ccx-0.5*size,cty+0.05*size,ccx+0.5*size,cty+0.05*size,ccx+0.5*size,cty);
	ctx.stroke();
	ctx.closePath();
	ctx.fillStyle = cc1;
	ctx.fill();
	
	ctx.lineWidth = 4;
	ctx.strokeStyle = cc2;
	ctx.beginPath();
	ctx.moveTo(ccx, cty-0.025*size);
	ctx.lineTo(ccx, cty-0.025*size-20);
	ctx.arc(ccx, cty-0.025*size-20-7, 10, Math.PI/2, Math.PI, true);
	ctx.stroke();
	ctx.closePath();
	
	
	
}

function DrawBart(x, y, w){
	ctx.lineWidth = 0.01*w;
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#fed31d";
	ctx.beginPath();
	ctx.moveTo(x, y);
/*  Spikey Hair */
	ctx.lineTo(x+0.09*w, y+0.10*w);
	ctx.lineTo(x+0.12*w, y+0.00*w);
	ctx.lineTo(x+0.20*w, y+0.08*w);
	ctx.lineTo(x+0.25*w, y-0.03*w);
	ctx.lineTo(x+0.30*w, y+0.07*w);
	ctx.lineTo(x+0.39*w, y-0.02*w);
	ctx.lineTo(x+0.43*w, y+0.06*w);
	ctx.lineTo(x+0.50*w, y+0.00*w);
	ctx.lineTo(x+0.57*w, y+0.08*w);
	ctx.lineTo(x+0.61*w, y+0.00*w);
	ctx.lineTo(x+0.69*w, y+0.11*w);
	ctx.lineTo(x+0.77*w, y+0.01*w);
	ctx.lineTo(x+0.80*w, y+0.12*w);
	ctx.lineTo(x+0.88*w, y+0.05*w);
	ctx.lineTo(x+0.90*w, y+0.17*w);
	ctx.lineTo(x+1.00*w, y+0.10*w);
	
	/*  Rest of head */
	ctx.lineTo(x+0.80*w, y+1.30*w);
	ctx.lineTo(x+0.85*w, y+1.57*w);

	ctx.bezierCurveTo(x+0.95*w,y+1.63*w,x+0.40*w,y+1.70*w,x+0.40*w,y+1.65*w);
	ctx.lineTo(x+0.40*w,y+1.55*w);
	ctx.bezierCurveTo(x+0.40*w,y+1.55*w,x+0.29*w,y+1.55*w,x+0.29*w,y+1.45*w);
	ctx.bezierCurveTo(x+0.29*w,y+1.45*w,x-0.10*w,y+1.50*w,x-0.10*w,y+1.40*w);
	
	ctx.lineTo(x, y+1.00*w);
	ctx.lineTo(x, y+0.85*w);
	ctx.bezierCurveTo(x-0.10*w, y+0.85*w, x-0.10*w, y+0.70*w, x, y+0.70*w);
	ctx.lineTo(x, y);
	
	
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	/*  Left Eye */
	ctx.fillStyle = "#FFFFFF";
	ctx.beginPath();
	ctx.arc(x+0.10*w, y+0.90*w, 0.20*w, 0, 2*Math.PI);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	ctx.fillStyle = "#000000";
	ctx.beginPath();
	ctx.arc(x+0.10*w, y+0.90*w, 0.02*w, 0, 2*Math.PI);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	/* Nose */
	ctx.fillStyle = "#fed31d";
	ctx.beginPath();
	ctx.moveTo(x+0.45*w, y+1.00*w);
	ctx.lineTo(x+0.00*w, y+1.00*w);
	ctx.arc(x+0.00*w, y+1.10*w, 0.10*w, 3*Math.PI/2, 1*Math.PI/2, true);
	ctx.lineTo(x+0.10*w, y+1.20*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	/*  Ear */
	ctx.beginPath();
	ctx.arc(x+0.80*w, y+1.15*w, 0.10*w, 2.4*Math.PI/2, 1.6*Math.PI/2);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	ctx.beginPath();
	ctx.arc(x+0.80*w, y+1.15*w, 0.05*w, 2.4*Math.PI/2, 3.7*Math.PI/2);
	ctx.stroke();
	ctx.closePath();
	
	
	/*  Right Eye */
	ctx.fillStyle = "#FFFFFF";
	ctx.beginPath();
	ctx.arc(x+0.45*w, y+0.90*w, 0.20*w, 0, 2*Math.PI);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	ctx.fillStyle = "#000000";
	ctx.beginPath();
	ctx.arc(x+0.50*w, y+0.90*w, 0.02*w, 0, 2*Math.PI);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	/* Smile */
	ctx.beginPath();
	ctx.moveTo(x+0.29*w,y+1.45*w);
	ctx.lineTo(x+0.61*w, y+1.40*w);
	ctx.stroke();
	ctx.closePath();
	
	/*  Legs */
	
	ctx.fillStyle = "#fed31d";
	ctx.fillRect(x+0.36*w, y+2.80*w, 0.27*w, 0.60*w);
	ctx.strokeRect(x+0.36*w, y+2.80*w, 0.27*w, 0.60*w);
	
	ctx.fillRect(x+0.65*w, y+2.80*w, 0.27*w, 0.60*w);
	ctx.strokeRect(x+0.65*w, y+2.80*w, 0.27*w, 0.60*w);
	
	/*  Left Arm */
	
	ctx.fillStyle = "#fed31d";
	ctx.fillRect(x+0.28*w, y+2.00*w, 0.20*w, 0.30*w);
	ctx.strokeRect(x+0.28*w, y+2.00*w, 0.20*w, 0.30*w);
	
	/*Pants */
	ctx.fillStyle = "#009cdb";
	ctx.beginPath();
	ctx.moveTo(x+0.60*w, y + 2.50*w);
	ctx.lineTo(x+0.60*w,y+2.85*w);
	ctx.lineTo(x+0.20*w,y+2.80*w);
	ctx.lineTo(x+0.20*w,y+2.50*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	ctx.beginPath();
	ctx.moveTo(x+1.20*w, y + 2.60*w);
	ctx.bezierCurveTo(x+1.20*w, y + 2.60*w, x+1.20*w,y+2.73*w, x+1.10*w,y+2.73*w);
	ctx.lineTo(x+1.10*w,y+2.90*w);
	ctx.lineTo(x+0.50*w,y+2.90*w);
	ctx.lineTo(x+0.50*w,y+2.80*w);
	ctx.bezierCurveTo(x+0.50*w,y+2.80*w, x+0.20*w,y+2.80*w, x+0.20*w,y+2.50*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();

	
	
	
	
	/*  Shirt */
	ctx.fillStyle = "#f04d28";

	ctx.beginPath();
	ctx.moveTo(x+0.25*w, y + 2.00*w);
	ctx.bezierCurveTo(x+0.25*w, y + 1.60*w, x+0.65*w,y+1.60*w, x+0.65*w,y+2.00*w);
	ctx.lineTo(x+0.25*w, y + 2.00*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	ctx.beginPath();
	ctx.moveTo(x+0.90*w, y + 1.55*w);
	ctx.lineTo(x+1.05*w, y + 2.10*w);
	ctx.bezierCurveTo(x+1.05*w, y + 2.10*w, x+1.20*w,y+2.10*w, x+1.20*w,y+2.60*w);
	ctx.bezierCurveTo(x+1.20*w,y+2.65*w, x+0.13*w,y+2.55*w, x+0.13*w,y+2.50*w);
	ctx.lineTo(x+0.30*w, y + 2.10*w);
	ctx.lineTo(x+0.40*w, y + 1.65*w);
	ctx.bezierCurveTo(x+0.40*w, y + 1.70*w, x+0.90*w, y + 1.60*w, x+0.90*w, y + 1.55*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	ctx.beginPath();
	ctx.moveTo(x+0.70*w, y + 2.00*w);
	ctx.bezierCurveTo(x+0.70*w, y + 1.60*w, x+1.10*w,y+1.60*w, x+1.10*w,y+2.00*w);
	ctx.bezierCurveTo(x+1.10*w, y + 1.70*w, x+0.70*w,y+1.70*w, x+0.70*w,y+2.00*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	/*  Left Shoes */
	ctx.fillStyle = "#009cdb";
	ctx.beginPath();
	ctx.moveTo(x+0.60*w, y + 3.26*w);
	ctx.quadraticCurveTo(x+0.70*w, y + 3.30*w, x+0.70*w, y + 3.53*w);
	ctx.lineTo(x+0.10*w, y + 3.53*w);
	ctx.quadraticCurveTo(x+0.10*w, y + 3.30*w, x+0.40*w, y + 3.30*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	ctx.fillStyle = "#ffffff";
	ctx.beginPath();
	ctx.moveTo(x+0.34*w, y + 3.20*w);
	ctx.bezierCurveTo(x+0.32*w,y + 3.25*w, x+0.63*w,y+3.25*w, x+0.63*w,y+3.20*w);
	ctx.lineTo(x+0.65*w, y + 3.30*w);
	ctx.bezierCurveTo(x+0.65*w,y + 3.35*w, x+0.34*w,y+3.35*w, x+0.34*w,y+3.30*w);
	ctx.lineTo(x+0.34*w, y + 3.20*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	ctx.beginPath();
	ctx.moveTo(x+0.70*w,y + 3.53*w);
	ctx.bezierCurveTo(x+0.70*w,y + 3.58*w, x+0.10*w,y+3.58*w, x+0.10*w,y+3.53*w);
	ctx.lineTo(x+0.10*w, y + 3.48*w);
	ctx.bezierCurveTo(x+0.10*w,y + 3.53*w, x+0.70*w,y+3.53*w, x+0.70*w,y+3.48*w);
	ctx.lineTo(x+0.70*w,y + 3.53*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	 
	ctx.beginPath();
	ctx.arc(x+0.60*w, y+3.42*w, 0.06*w, 0, 2*Math.PI);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	/*  Right Shoes */
	ctx.fillStyle = "#009cdb";
	ctx.beginPath();
	ctx.moveTo(x+0.90*w, y + 3.36*w);
	ctx.quadraticCurveTo(x+1.00*w, y + 3.40*w, x+1.00*w, y + 3.63*w);
	ctx.lineTo(x+0.40*w, y + 3.63*w);
	ctx.quadraticCurveTo(x+0.40*w, y + 3.40*w, x+0.70*w, y + 3.40*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	
	ctx.fillStyle = "#ffffff";
	ctx.beginPath();
	ctx.moveTo(x+0.64*w, y + 3.30*w);
	ctx.bezierCurveTo(x+0.62*w,y + 3.35*w, x+0.93*w,y+3.35*w, x+0.93*w,y+3.30*w);
	ctx.lineTo(x+0.95*w, y + 3.40*w);
	ctx.bezierCurveTo(x+0.95*w,y + 3.45*w, x+0.64*w,y+3.45*w, x+0.64*w,y+3.40*w);
	ctx.lineTo(x+0.64*w, y + 3.30*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	
	ctx.beginPath();
	ctx.moveTo(x+1.00*w,y + 3.63*w);
	ctx.bezierCurveTo(x+1.00*w,y + 3.68*w, x+0.40*w,y+3.68*w, x+0.40*w,y+3.63*w);
	ctx.lineTo(x+0.40*w, y + 3.58*w);
	ctx.bezierCurveTo(x+0.40*w,y + 3.63*w, x+1.00*w,y+3.63*w, x+1.00*w,y+3.58*w);
	ctx.lineTo(x+1.00*w,y + 3.63*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();

	/* Right Arm */
	ctx.fillStyle = "#fed31d";
	ctx.beginPath();
	ctx.moveTo(x+0.70*w, y + 2.00*w);
	ctx.bezierCurveTo(x+0.70*w, y + 1.60*w, x+1.10*w,y+1.60*w, x+1.10*w,y+2.00*w);
	ctx.bezierCurveTo(x+1.10*w, y + 2.20*w, x+0.70*w,y+2.20*w, x+0.70*w,y+2.00*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();

	ctx.beginPath();
	ctx.moveTo(x+0.70*w,y+2.00*w);
	ctx.lineTo(x+0.83*w,y+2.00*w);
	
	ctx.moveTo(x+0.85*w,y+2.15*w);
	ctx.lineTo(x+0.85*w,y+2.00*w);
	
	ctx.moveTo(x+0.95*w,y+2.15*w);
	ctx.lineTo(x+0.86*w,y+2.00*w);
	
	ctx.stroke();
	ctx.closePath();
	
	ctx.fillStyle = "#f04d28";
	ctx.beginPath();
	ctx.moveTo(x+0.70*w, y + 2.00*w);
	ctx.bezierCurveTo(x+0.70*w, y + 1.60*w, x+1.10*w,y+1.60*w, x+1.10*w,y+2.00*w);
	ctx.bezierCurveTo(x+1.10*w, y + 1.70*w, x+0.70*w,y+1.70*w, x+0.70*w,y+2.00*w);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
}

function LabQuestOnOne(x,y,hw,lqh,lqc, u){

	/* Calculates the time to go on the LabQuest display */
	
	d = new Date();
	
	h = d.getHours();
	m = d.getMinutes();
	
	z = "AM";
	
	if (h == 0){
		h = 12;
	}
	if (h > 12){
		h = h - 12;
		z = "PM";
	}
	
	if (h < 10){
		h = "0" + h;
	}
	
	if (m < 10){
		m = "0" + m;
	}
	
	displaytime = h + ":" + m  + z;
	
	/*  Generates a small random error to make display reading flicker */
	
	if (u != 1){
		ED = Math.random()*2;
		
		if (ED > 1){
			ErrorDirection = 1;
		}
		else{
			ErrorDirection = -1;
		}
		
		if (Force > 1){
			Error = Math.random()*Force*0.002*ErrorDirection;
		}
		else{
			Error = Math.random()*Force*.01*ErrorDirection;
		}
		
		DisplayForce = Force+Error;
		
		/*  Makes sure the displayed force is not larger than Maximum Value */
		
		if (Force > 50){
			Force = 50;
		}
	}
	else{
		DisplayForce = Force;
	}
	
	/*  Draws the body of the Lab Quest */
	
	ctx.lineWidth = 1;
	ctx.strokeStyle = "#444444";
	ctx.fillStyle = "#444444";
	ctx.beginPath();
	ctx.moveTo(x-hw+lqc,y);
	ctx.arc(x-hw+lqc, y-lqc, lqc, 0.5*Math.PI, 1*Math.PI);
	ctx.lineTo(x-hw,y-lqh+lqc);
	ctx.arc(x-hw+lqc, y-lqh+lqc, lqc, 1*Math.PI, 1.5*Math.PI);
	ctx.lineTo(x+hw-lqc,y-lqh);
	ctx.arc(x+hw-lqc, y-lqh+lqc, lqc, 1.5*Math.PI, 0*Math.PI);
	ctx.lineTo(x+hw,y-lqc);
	ctx.arc(x+hw-lqc, y-lqc, lqc, 0*Math.PI, 0.5*Math.PI);
	ctx.lineTo(x-hw+lqc,y);
	ctx.closePath();
	ctx.fill();
	ctx.stroke();
	
	/*  Rectangles on Screen */
	
	ctx.fillStyle = "#49999e";
	ctx.fillRect(x-0.75*hw,y-0.825*lqh,1.5*hw,0.75*lqh);
		
	ctx.fillStyle = "#397b7d";
	ctx.strokeStyle = "#ffffff";
	ctx.fillRect(x-0.7425*hw,y-0.75*lqh,1.01*hw,0.625*lqh);
	ctx.strokeRect(x-0.7425*hw,y-0.75*lqh,1.01*hw,0.625*lqh);
	
	ctx.fillStyle = "#ea212c";
	ctx.strokeStyle = "#ffffff";
	ctx.fillRect(x-0.7375*hw,y-0.6875*lqh,1.0*hw,0.5*lqh);
	ctx.strokeRect(x-0.7375*hw,y-0.6875*lqh,1.0*hw,0.5*lqh);
	
	ctx.fillStyle = "#555555";
	ctx.fillRect(x-0.75*hw,y-0.125*lqh,1.5*hw,0.065*lqh);
	
	ctx.fillStyle = "#FFFFFF";
	ctx.strokeStyle = "#000000";
	ctx.fillRect(x-0.725*hw,y-0.82*lqh,0.065*lqh,0.065*lqh);
	ctx.strokeRect(x-0.725*hw,y-0.82*lqh,0.065*lqh,0.065*lqh);
	
	ctx.fillStyle = "#c2d6d8";
	ctx.fillRect(x+0.2875*hw,y-0.75*lqh,0.45*hw,0.35*lqh);
	
	ctx.strokeStyle = "#49999e";
	ctx.strokeRect(x+0.3*hw,y-0.7375*lqh,0.425*hw,0.1*lqh);
	ctx.strokeRect(x+0.3*hw,y-0.625*lqh,0.425*hw,0.1*lqh);
	ctx.strokeRect(x+0.3*hw,y-0.5125*lqh,0.425*hw,0.1*lqh);
	
	/*  Words on Lab Quest */
	
	/*  Vernier */
	
	ctx.font = 0.1*lqh + "px Arial";
	ctx.fillStyle="#FFFFFF";
	temptext = "Vernier";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	ctx.save();
	ctx.translate(x,y-100);
	ctx.rotate(-0.5*Math.PI);
	ctx.fillText(temptext,-0.5*textWidth,-0.83*hw);
	ctx.restore();


	
	/* Lab Quest */
	
	ctx.font= 0.1*lqh + "px Arial";
	ctx.fillStyle="#49999e";
	temptext = "LabQuest ®";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x-0.375*hw-0.5*textWidth;
	ctx.fillText(temptext,xposition, y-0.875*lqh);
	
	ctx.fillStyle="#ea212c";
	temptext = "2";
	xposition = x-0.3625*hw+0.5*textWidth;
	ctx.fillText(temptext,xposition, y-0.875*lqh);
	
	
	/* Connected Science System */
	ctx.font = 0.05*lqh + "px Arial";
	ctx.fillStyle="#FFFFFF";
	temptext = "CONNECTED SCIENCE SYSTEM";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.35*hw-0.5*textWidth;
	ctx.fillText(temptext,xposition, y-0.0125*lqh);
	
	
	/* Force Channel 1 */
	ctx.font = 0.075*lqh + "px Arial bold";
	ctx.fillStyle="#FFFFFF";
	temptext = "CH 1: FORCE";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x-0.475*hw-0.5*textWidth;
	ctx.fillText(temptext,xposition, y-0.575*lqh);
	

	/* Force Reading */
	ctx.font = 0.15*lqh + "px Arial bold";
	ctx.fillStyle="#FFFFFF";
	if (u != 1){
		temptext = DisplayForce.toFixed(2) + " N";
	}
	else{
		temptext = DisplayForce.toPrecision(2) + " N";
	}
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.125*hw-textWidth;
	ctx.fillText(temptext,xposition, y-0.375*lqh);
	
	/* Display Time */
	ctx.font = 0.035*lqh + "px Arial";
	ctx.fillStyle="#FFFFFF";
	temptext = displaytime;
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.725*hw-textWidth;
	ctx.fillText(temptext,xposition, y-0.0875*lqh);
	
	/* Menus */
	ctx.font = 0.035*lqh + "px Arial";
	ctx.fillStyle="#FFFFFF";
	temptext = "File    Sensors";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x-0.625*hw;
	ctx.fillText(temptext,xposition, y-0.775*lqh);
	
	/* Data Collection Options */
	ctx.font = 0.035*lqh + "px Arial Bold";
	ctx.fillStyle="#000000";
	temptext = "Mode:";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.3125*hw;
	ctx.fillText(temptext,xposition, y-0.7*lqh);
	
	temptext = "Rate:";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.3125*hw;
	ctx.fillText(temptext,xposition, y-0.5875*lqh);
	
	temptext = "Length:";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.3125*hw;
	ctx.fillText(temptext,xposition, y-0.475*lqh);
	
	ctx.font = 0.035*lqh + "px Arial";
	ctx.fillStyle="#000000";
	temptext = "Time Based";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.5125*hw - 0.5*textWidth;
	ctx.fillText(temptext,xposition, y-0.6625*lqh);
	
	temptext = "1.0 samples/s";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.5125*hw - 0.5*textWidth;
	ctx.fillText(temptext,xposition, y-0.55*lqh);
	
	temptext = "180.0 s";
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x+0.5125*hw - 0.5*textWidth;
	ctx.fillText(temptext,xposition, y-0.4375*lqh);
	
}
