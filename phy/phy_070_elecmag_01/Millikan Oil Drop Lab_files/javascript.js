dropradius = new Array();
dropcharge = new Array();
dropx = new Array();
dropy = new Array();
speedx = new Array();
speedy = new Array();
evaporationrate = new Array();
accelerationy = new Array();	

/* called by onLoad */

function initialize(){
	TankHeight = 300;
	TankWidth = 350;
	TankStartX = 200;
	TankStartY = 200;
	TankSlant = 50;
	fanstatus = "off";
	lightstatus = "off";
	esensorstatus = "off";
	PlateSeparationmm = 3;
	PlateSeparationm = PlateSeparationmm/1000;
	DensityOfDrop = 900;
	Voltage = 10;
	DropsOut = "no";
	Viewing = "Main";
	Viewed = "No";
	theCanvas = document.getElementById("CanvasOne");
	ctx = theCanvas.getContext("2d");
		
}


/* Called by the Begin Button */

function LoadIt(){
	document.getElementById("LabSection").style.visibility = "visible";
	document.getElementById("OverviewSection").style.visibility = "hidden";
	document.getElementById("ViewDropButton").style.visibility = "hidden";
	StartItMoving = setInterval(drawingpart, 20);
	
}

function SpritzOilData(){
	Voltage = Math.floor(Math.random()*100 + 100)/10;
	ElectricField = Voltage/PlateSeparationm;
	for (i = 0; i<100; i++){
		dropradius[i] = Math.random()*50 + 20;
		dropcharge[i] = Math.floor(Math.random()*9+0);
		dropx[i] = 200;
		dropy[i] = 375;
		speedx[i] = 20;
		speedy[i] = 0;
		radiusinm = dropradius[i]/1e8;
		Volumedrop = 4/3*Math.PI*Math.pow(radiusinm, 3);
		Massdrop = Volumedrop*DensityOfDrop;
		ForceGravityDrop = Massdrop*9.8;
		ForceElectric = ElectricField*dropcharge[i]*1.602e-19;
		NetForce = ForceGravityDrop - ForceElectric;
		accelerationy[i] = NetForce/Massdrop/100;
		evaporationrate[i] = 0;
	}
	magicdrop = Math.floor(Math.random()*99);
	ForceElectricMagicDrop = 1.602e-19*dropcharge[magicdrop]*ElectricField;
	ForceGravityMagicDrop = ForceElectricMagicDrop;
	MassOfMagicDrop = ForceGravityMagicDrop/9.8;
	VolumeOfMagicDrop = MassOfMagicDrop/DensityOfDrop;
	Garbage = VolumeOfMagicDrop*3/(4*Math.PI);
	RadiusOfMagicDrop = Math.pow(Garbage, (1/3));
	dropradius[magicdrop] = RadiusOfMagicDrop*1e6*100;
	accelerationy[magicdrop] = 0;
}

function SprayOil(){
	SpritzOilData();
	DropsOut = "yes";
	Viewed = "No";
	StartDrop = 0;
	document.getElementById("SpritzButton").style.visibility = "hidden";
	document.getElementById("ViewDropButton").style.visibility = "hidden";
	document.getElementById("ViewMainButton").style.visibility = "hidden";
}






function drawingpart(){
			
	/* 	background drawing */

	ctx.fillStyle="#FFFFFF";
	ctx.fillRect(0,0,900,600);
	
	if (Viewing == "Main"){
		//DrawLabTable(-100, 500, 900, 40);
		DrawPlates(200, 375, 200, 10);
			
		DrawSprayer(125,300,200);
		DrawMagnifier(390, 380, 200, 50);
		
		DrawBottomFrontOfApparatus(300,460, 210, 40);
		DrawGlassFrontOfApparatus(300,280, 210, 180);
		DrawTopFrontOfApparatus(300,260, 210, 40);
		
		if (DropsOut == "yes"){
			for (i = 0; i <= StartDrop; i++){
				dropx[i] = dropx[i] + 0.3*speedx[i];
				speedx[i] = speedx[i] - 0.5;
				speedy[i] = speedy[i] + accelerationy[i];
				dropy[i] = dropy[i] + speedy[i];
				if (speedx[i] < 0){
					speedx[i] = 0;
				}
				if (dropy[i]<310){
					dropy[i]=310;
					speedy[i] = 0;
					accelerationy[i] = 0;
					speedx[i] = 0;
					evaporationrate[i] = 0.05;
				}
				if (dropy[i]>450){
					
					dropy[i]=450;
					speedy[i] = 0;
					accelerationy[i] = 0;
					speedx[i] = 0;
					evaporationrate[i] = 0.05;
				}
				dropradius[i] = dropradius[i] - evaporationrate[i];
				if (dropradius[i] < 0){
					dropradius[i] = 0;
				}
				DrawDrop(dropx[i], dropy[i], dropradius[i]*3);
			}
			StartDrop++;
			if (StartDrop > 100){
				StartDrop = 100;
				document.getElementById("SpritzButton").style.visibility = "visible";
				document.getElementById("ViewDropButton").style.visibility = "visible";
				document.getElementById("ViewMainButton").style.visibility = "hidden";	
			}
			if (Viewed == "No"){
				WriteText(450,50,"Püskürtücüden çıkarken damlalar eksi yük kazanacaklar",22,"#990000",0.5);
				WriteText(450,100,"Bazı damlaların yukarı diğerlerinin aşağı gittiğine dikkat",22,"#990000",0.5);
				WriteText(450,150,"Eğer askıda kalan damla hiç yoksa  tekrar püskürtme yapın.",22,"#990000",0.5);
				WriteText(450,200,"Askıda kalan damla varsa, mikroskop merceğine tıklayın ve onu büyütülmüş halde görün.",22,"#990000",0.5);
			}
			else{
				WriteText(450,50,"Eğer bu damla üzerindeki yükü hesapladıysanız,",22,"#990000",0.5);
				WriteText(450,100,"daha fazla yağ püskürtün ve aynı hesapları yeni bir damla üzerinde tekrarlayın.",22,"#990000",0.5);
				WriteText(450,150,"Bu şekilde en az 10 damla üzerinde veri toplayın.",22,"#990000",0.5);
				WriteText(450,200,"Sonra verinizde bir kalıp var mı ona bakın.",22,"#990000",0.5);
			}
			
			ctx.strokeStyle = "#000000";
			ctx.lineWidth = 2;
			ctx.strokeRect(600, 250, 300, 300);
			WriteText(750,300,"Dengedeki Kuvvetler",22,"#990000",0.5);
			DrawDrop(750,425,50*100);
			DrawArrow(750,325, 75, 25, "#990000", 0);
			WriteText(825,350,"Fe = qE",22,"#990000",0.5);
			
			DrawArrow(750,450, 75, 25, "#000099", Math.PI);
			WriteText(825,500,"Fg = mg",22,"#000099",0.5);
		}
		else{
			WriteText(450,50,"Fışkırtıcının yuvarlak püskürtücüsünü sıkın (tıklayarak)",22,"#990000",0.5);
		}
	}
	else{
		ctx.fillStyle="#000000";
		ctx.fillRect(0,0,900,600);
		
		
		ctx.fillStyle = "#FFFFFF";
		ctx.beginPath();
		ctx.arc(300,300, 280, 0, 2*Math.PI);
		ctx.fill();
		
		ctx.strokeStyle = "#000000";
		startx = 300;
		starty = 300;
		
		DrawDrop(startx,starty,dropradius[magicdrop]*400);
		
		ctx.lineWidth = 1;
		ctx.beginPath();
		ctx.moveTo(startx - 270, starty);
		ctx.lineTo(startx + 270, starty);
		ctx.stroke();
		
		ctx.beginPath();
		ctx.moveTo(startx, starty - 270);
		ctx.lineTo(startx, starty + 270);
		ctx.stroke();
		inter = 4;
		for (i = 0; i < 68; i++){
			if (i%10 == 0){
				jump = 15;
				ctx.lineWidth = 1;
				if (i > 5){
					WriteText(startx-i*inter,starty + 40,i*10,14,"#000000",0.5);
					WriteText(startx+i*inter,starty + 40,i*10,14,"#000000",0.5);
				}
				
			}
			else if (i%5 == 0){
				jump = 10;
				ctx.lineWidth = 1;
			}
			else{
				jump = 7;
				ctx.lineWidth = 0.5;
			}
			ctx.beginPath();
			ctx.moveTo(startx+i*inter, starty - jump);
			ctx.lineTo(startx+i*inter, starty + jump);
			ctx.stroke();
			
			ctx.beginPath();
			ctx.moveTo(startx-i*inter, starty - jump);
			ctx.lineTo(startx-i*inter, starty + jump);
			ctx.stroke();
			
			ctx.beginPath();
			ctx.moveTo(startx - jump, starty-i*inter);
			ctx.lineTo(startx + jump, starty-i*inter);
			ctx.stroke();
			
			ctx.beginPath();
			ctx.moveTo(startx - jump, starty+i*inter);
			ctx.lineTo(startx + jump, starty+i*inter);
			ctx.stroke();
		}
		
		
		WriteText(startx + 20,starty + 230,"scale in nm",20,"#000000",0);
		
		WriteText(700,50,"Şu anda dengede duran damlaya bakıyorsunuz",16,"#FFFFFF",0.5);
		WriteText(710,100,"Damlanın çapını ölçün",16,"#FFFFFF",0.5);
		WriteText(720,150,"Yağın yoğunluğu " + DensityOfDrop.toFixed(0) + " kg/m^3",16,"#FFFFFF",0.5);
		WriteText(730,200,"Damlanın hacmini ve kütlesini hesapla",16,"#FFFFFF",0.5);
		WriteText(740,250,"Damla üzerinde Fg hesapla",16,"#FFFFFF",0.5);
		WriteText(750,300,"Fe = Fg yap",16,"#FFFFFF",0.5);
		WriteText(740,350,"Kapasitör plakaları arası " + PlateSeparationmm.toFixed(1) + " mm ",16,"#FFFFFF",0.5);
		WriteText(730,400,"Plakalar arasındaki ∆V  " + Voltage.toFixed(1) + " V",16,"#FFFFFF",0.5);
		WriteText(720,450,"Plakalar arasındaki elektrik alanını hesapla.",16,"#FFFFFF",0.5);
		WriteText(710,500,"Yağ damlasının yükünü hesapla.",16,"#FFFFFF",0.5);
		WriteText(700,550,"Başa dön",24,"#FFFFFF",0.5);
	}
}

function DrawDrop(x,y,r){
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#de8003";
	ctx.lineWidth = 0.1;
	ctx.beginPath();
	ctx.arc(x, y, r/100, 0, 2*Math.PI);
	ctx.fill();
}

function DrawMagnifier(x,y,w,h){
	
	// Back Eyepiece
	
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#222222";
	ctx.lineWidth = 2;
	ctx.beginPath();
	ctx.moveTo(x+0.94*w, y-0.17*h);
	ctx.bezierCurveTo(x+0.92*w, y-0.17*h, x+0.92*w, y+0.17*h, x+0.94*w, y+0.17*h);
	ctx.lineTo(x+0.98*w, y+0.17*h);
	ctx.bezierCurveTo(x+1.0*w, y+0.17*h, x+1.0*w, y-0.17*h, x+0.98*w, y-0.17*h);
	ctx.lineTo(x+0.94*w, y-0.17*h);
	ctx.stroke();
	ctx.fill();
	
	//right side
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#444444";
	ctx.lineWidth = 2;
	ctx.beginPath();
	ctx.moveTo(x+0.49*w, y-0.4*h);
	ctx.lineTo(x+0.49*w, y+0.4*h);
	ctx.lineTo(x+0.95*w, y+0.15*h);
	ctx.lineTo(x+0.95*w, y-0.15*h);
	ctx.lineTo(x+0.49*w, y-0.4*h);
	ctx.stroke();
	ctx.fill();
		
	//left side
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#333333";
	ctx.lineWidth = 2;
	ctx.beginPath();
	ctx.moveTo(x+0.15*w, y-0.48*h);
	ctx.lineTo(x+0.15*w, y+0.48*h);
	ctx.lineTo(x+0.50*w, y+0.4*h);
	ctx.lineTo(x+0.50*w, y-0.4*h);
	ctx.lineTo(x+0.15*w, y-0.48*h);
	ctx.stroke();
	ctx.fill();
	
	
	
	//Eye Piece
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#222222";
	ctx.lineWidth = 2;
	ctx.beginPath();
	ctx.moveTo(x, y-0.5*h);
	ctx.bezierCurveTo(x-0.05*w, y-0.5*h, x-0.05*w, y+0.5*h, x, y+0.5*h);
	ctx.lineTo(x+0.15*w, y+0.5*h);
	ctx.bezierCurveTo(x+0.20*w, y+0.5*h, x+0.20*w, y-0.5*h, x+0.15*w, y-0.5*h);
	ctx.lineTo(x, y-0.5*h);
	ctx.stroke();
	ctx.fill();
	
	// Glass 
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#94ded5";
	ctx.lineWidth = 2;
	ctx.beginPath();
	ctx.moveTo(x, y-0.5*h);
	ctx.bezierCurveTo(x-0.05*w, y-0.5*h, x-0.05*w, y+0.5*h, x, y+0.5*h);
	ctx.bezierCurveTo(x+0.05*w, y+0.5*h, x+0.05*w, y-0.5*h, x, y-0.5*h);
	ctx.stroke();
	ctx.fill();
		
}


function DrawBottomFrontOfApparatus(x,y,w,h){
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#333333";
	ctx.lineWidth = 1;
	ctx.beginPath();
	ctx.moveTo(x-0.5*w, y);
	ctx.bezierCurveTo(x-0.5*w, y-0.05*h, x+0.5*w, y-0.05*h, x+0.5*w, y);
	ctx.lineTo(x+0.5*w, y+h);
	ctx.bezierCurveTo(x+0.5*w, y+1.05*h, x-0.5*w, y+1.05*h, x-0.5*w, y+h);
	ctx.lineTo(x-0.5*w, y);
	ctx.fill();
	ctx.strokeStyle = "#333333";
	ctx.fillStyle = "#C0c0c0";
	ctx.fillRect(x-0.25*w, y+0.2*h, 0.5*w, 0.7*h);
	WriteText(x,y+0.7*h,Voltage + " V",16,"#990000",0.5);
	
}

function DrawTopFrontOfApparatus(x,y,w,h){
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#333333";
	ctx.lineWidth = 1;
	ctx.beginPath();
	ctx.moveTo(x-0.5*w, y);
	ctx.bezierCurveTo(x-0.5*w, y-0.05*h, x+0.5*w, y-0.05*h, x+0.5*w, y);
	ctx.lineTo(x+0.5*w, y+h);
	ctx.bezierCurveTo(x+0.5*w, y+1.05*h, x-0.5*w, y+1.05*h, x-0.5*w, y+h);
	ctx.lineTo(x-0.5*w, y);
	ctx.fill();
	WriteText(x,y+0.4*h,"Millikan Oil Drop Apparatus",14,"#CCCCCC",0.5);
	WriteText(x,y+0.8*h,"Made in Chicago",9,"#CCCCCC",0.5);
}

function DrawGlassFrontOfApparatus(x,y,w,h){
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#94ded5";
	ctx.lineWidth = 1;
	ctx.beginPath();
	ctx.moveTo(x-0.5*w, y);
	ctx.bezierCurveTo(x-0.5*w, y-0.05*h, x+0.5*w, y-0.05*h, x+0.5*w, y);
	ctx.lineTo(x+0.5*w, y+h);
	ctx.bezierCurveTo(x+0.5*w, y+1.02*h, x-0.5*w, y+1.02*h, x-0.5*w, y+h);
	ctx.lineTo(x-0.5*w, y);
	ctx.globalAlpha = 0.4;
	
	ctx.fill();
	ctx.globalAlpha = 1.0;
}

function DrawSprayer(x,y,w){
	ss = w/100;
	
	// Back of Bottle
	
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#e2ffef";
	ctx.lineWidth = 1*ss;
	ctx.beginPath();
	ctx.moveTo(x, y+50*ss);
	ctx.lineTo(x+5*ss, y+50*ss);
	ctx.lineTo(x+5*ss, y+55*ss);
	ctx.lineTo(x+20*ss, y+80*ss);
	ctx.lineTo(x+20*ss, y+100*ss);
	ctx.lineTo(x-20*ss, y+100*ss);
	ctx.lineTo(x-20*ss, y+80*ss);
	ctx.lineTo(x-5*ss, y+55*ss);
	ctx.lineTo(x-5*ss, y+50*ss);
	ctx.lineTo(x, y+50*ss);
	ctx.stroke();
	ctx.fill();
	
	// Spritz Tube
	
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#de8003";
	ctx.lineWidth = 1*ss;
	ctx.beginPath();
	ctx.moveTo(x+2*ss, y+95*ss);
	ctx.lineTo(x+2*ss, y+42*ss);
	ctx.lineTo(x+35*ss, y+42*ss);
	ctx.lineTo(x+40*ss, y+40*ss);
	
	ctx.moveTo(x-2*ss, y+95*ss);
	ctx.lineTo(x-2*ss, y+42*ss);
	ctx.lineTo(x-15*ss, y+42*ss);
	ctx.lineTo(x-20*ss, y+40*ss);
	
	ctx.moveTo(x+40*ss, y+38*ss);
	ctx.lineTo(x+35*ss, y+36*ss);
	ctx.lineTo(x-15*ss, y+36*ss);
	ctx.lineTo(x-20*ss, y+38*ss);
	
	ctx.stroke();
	ctx.beginPath();
	
	ctx.moveTo(x+2*ss, y+95*ss);
	ctx.lineTo(x+2*ss, y+42*ss);
	ctx.lineTo(x+35*ss, y+42*ss);
	ctx.lineTo(x+40*ss, y+40*ss);
	ctx.lineTo(x+40*ss, y+38*ss);
	ctx.lineTo(x+35*ss, y+36*ss);
	ctx.lineTo(x-15*ss, y+36*ss);
	ctx.lineTo(x-20*ss, y+38*ss);
	ctx.lineTo(x-20*ss, y+40*ss);
	ctx.lineTo(x-15*ss, y+42*ss);
	ctx.lineTo(x-2*ss, y+42*ss);
	ctx.lineTo(x-2*ss, y+95*ss);
	ctx.lineTo(x+2*ss, y+95*ss);
	ctx.globalAlpha = 0.7;
	ctx.fill();
	ctx.globalAlpha = 1.0;
	
	
	
	// Oil in Bottle
	
	ctx.fillStyle = "#de8003";
	ctx.beginPath();
	ctx.moveTo(x+10*ss, y+64*ss);
	ctx.lineTo(x+20*ss, y+80*ss);
	ctx.lineTo(x+20*ss, y+100*ss);
	ctx.lineTo(x-20*ss, y+100*ss);
	ctx.lineTo(x-20*ss, y+80*ss);
	ctx.lineTo(x-10*ss, y+64*ss);
	ctx.globalAlpha = 0.7;
	ctx.fill();
	ctx.globalAlpha = 1.0;
	
	// Front of Bottle
	
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#e2ffef";
	ctx.lineWidth = 1*ss;
	ctx.beginPath();
	ctx.moveTo(x, y+50*ss);
	ctx.lineTo(x+5*ss, y+50*ss);
	ctx.lineTo(x+5*ss, y+55*ss);
	ctx.lineTo(x+20*ss, y+80*ss);
	ctx.lineTo(x+20*ss, y+100*ss);
	ctx.lineTo(x-20*ss, y+100*ss);
	ctx.lineTo(x-20*ss, y+80*ss);
	ctx.lineTo(x-5*ss, y+55*ss);
	ctx.lineTo(x-5*ss, y+50*ss);
	ctx.lineTo(x, y+50*ss);
	ctx.stroke();
	ctx.globalAlpha = 0.3;
	ctx.fill();
	ctx.globalAlpha = 1.0;
	
	// Air Sack
	
	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#C0c0c0";
	ctx.lineWidth = 2*ss;
	ctx.beginPath();
	ctx.arc(x-30*ss, y + 39*ss, 13*ss, 0, 2*Math.PI);
	
	ctx.stroke();
	ctx.fill();
	
}

function DrawPlates(x,y,w,h){

	ctx.strokeStyle = "#000000";
	ctx.fillStyle = "#c0c0c0";
	ctx.lineWidth = 1;
	ctx.strokeRect(x,y-75,w,h);
	ctx.fillRect(x,y-75,w,h);
	
	ctx.strokeRect(x,y+75,w,h);
	ctx.fillRect(x,y+75,w,h);
	
	for (i = 0; i < 10; i++){
		WriteText(x+20*i+10,y-65,"+",14,"#000000",0.5);
		WriteText(x+20*i+10,y+85,"-",14,"#000000",0.5);
	}
	
}

function ViewWindow(x){
	if (x == 1){
		Viewing = "Drop";
		document.getElementById("SpritzButton").style.visibility = "hidden";
		document.getElementById("ViewDropButton").style.visibility = "hidden";
		document.getElementById("ViewMainButton").style.visibility = "visible";
		Viewed = "Yes";
	}
	else{
		Viewing = "Main";
		document.getElementById("SpritzButton").style.visibility = "visible";
		document.getElementById("ViewDropButton").style.visibility = "visible";
		document.getElementById("ViewMainButton").style.visibility = "hidden";
	}
}

function WriteText(x,y,t,s,c,m){
	ctx.fillStyle = c;
	ctx.font= s + "px Arial";
	temptext = t;
	metrics = ctx.measureText(temptext);
	textWidth = metrics.width;
	xposition = x - m*textWidth;
	ctx.fillText(temptext,xposition, y);
}

function DrawArrow(x, y, h, w, c, r){
	ctx.save();
	ctx.translate(x, y+0.5*h);
	ctx.rotate(r);
	ctx.fillStyle = c;
	ctx.strokeStyle = "#000000";
	ctx.lineWidth = 1;
	ctx.beginPath();
	ctx.moveTo(0,-0.5*h);
	ctx.lineTo(0+w, -0.5*h+w);
	ctx.lineTo(0+0.5*w, -0.5*h+w);
	ctx.lineTo(0+0.5*w, -0.5*h+h);
	ctx.lineTo(0-0.5*w, -0.5*h+h);
	ctx.lineTo(0-0.5*w, -0.5*h+w);
	ctx.lineTo(0-w, -0.5*h+w);
	ctx.lineTo(0,-0.5*h);
	ctx.stroke();
	ctx.closePath();
	ctx.fill();
	ctx.restore();
}

