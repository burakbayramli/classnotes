void drawBox(float c[3], float width, float height, float depth){
	float w = width;
	float h = height;
	float d = depth;

	float verticies[8][3] = {{c[0]-w,c[1]-h,c[2]+d},   {c[0]-w,c[1]+h,c[2]+d},   {c[0]+w,c[1]+h,c[2]+d},   {c[0]+w,c[1]-h,c[2]+d},
							{c[0]-w,c[1]-h,c[2]-d},    {c[0]-w,c[1]+h,c[2]-d},   {c[0]+w,c[1]+h,c[2]-d},   {c[0]+w,c[1]-h,c[2]-d}};
	
	glBegin(GL_POLYGON);
	glVertex3fv(verticies[2]);
	glVertex3fv(verticies[6]);
	glVertex3fv(verticies[7]);
	glVertex3fv(verticies[3]);
	glEnd();
	
	glBegin(GL_POLYGON);
	glVertex3fv(verticies[0]);
	glVertex3fv(verticies[1]);
	glVertex3fv(verticies[2]);
	glVertex3fv(verticies[3]);
	glEnd();

	glBegin(GL_POLYGON);
	glVertex3fv(verticies[3]);
	glVertex3fv(verticies[7]);
	glVertex3fv(verticies[4]);
	glVertex3fv(verticies[0]);
	glEnd();

	glBegin(GL_POLYGON);
	glVertex3fv(verticies[5]);
	glVertex3fv(verticies[1]);
	glVertex3fv(verticies[2]);
	glVertex3fv(verticies[6]);
	glEnd();

	glBegin(GL_POLYGON);
	glVertex3fv(verticies[5]);
	glVertex3fv(verticies[4]);
	glVertex3fv(verticies[7]);
	glVertex3fv(verticies[6]);
	glEnd();

	glBegin(GL_POLYGON);
	glVertex3fv(verticies[0]);
	glVertex3fv(verticies[4]);
	glVertex3fv(verticies[5]);
	glVertex3fv(verticies[1]);
	glEnd();

	
}

void drawRoom(float c[3], float width, float height, float depth){
	float w = width;
	float h = height;
	float d = depth;

	float verticies[8][3] = {{c[0]-w,c[1]-h,c[2]+d},   {c[0]-w,c[1]+h,c[2]+d},   {c[0]+w,c[1]+h,c[2]+d},   {c[0]+w,c[1]-h,c[2]+d},
							{c[0]-w,c[1]-h,c[2]-d},    {c[0]-w,c[1]+h,c[2]-d},   {c[0]+w,c[1]+h,c[2]-d},   {c[0]+w,c[1]-h,c[2]-d}};
	
	glNormal3f(0,0,1);
	glBegin(GL_POLYGON);
	glColor4f(1,0,0,1);
	glVertex3fv(verticies[2]);
	glVertex3fv(verticies[6]);
	glVertex3fv(verticies[7]);
	glVertex3fv(verticies[3]);
	glEnd();
	
	glNormal3f(0,1,0);
	glBegin(GL_POLYGON);
	//glColor3f(0.3,0.2,1);
	glVertex3fv(verticies[0]);
	glVertex3fv(verticies[1]);
	glVertex3fv(verticies[2]);
	glVertex3fv(verticies[3]);
	glEnd();

	glNormal3f(-1,0,0);
	glBegin(GL_POLYGON);
//	glColor3f(0.2,0.2,.9);
	glVertex3fv(verticies[3]);
	glVertex3fv(verticies[7]);
	glVertex3fv(verticies[4]);
	glVertex3fv(verticies[0]);
	glEnd();

	glNormal3f(1,0,0);
	glBegin(GL_POLYGON);
	//glColor4f(0.2,0.1,1,0.1);
	glVertex3fv(verticies[5]);
	glVertex3fv(verticies[1]);
	glVertex3fv(verticies[2]);
	glVertex3fv(verticies[6]);
	glEnd();

	glNormal3f(0,0,1);
	glBegin(GL_POLYGON);
	//glColor3f(0.2,0.2,.8);
	glVertex3fv(verticies[5]);
	glVertex3fv(verticies[4]);
	glVertex3fv(verticies[7]);
	glVertex3fv(verticies[6]);
	glEnd();

	glNormal3f(0,-1,0);
	glBegin(GL_POLYGON);
	//glColor3f(0.2,0.2,1);
	glVertex3fv(verticies[0]);
	glVertex3fv(verticies[4]);
	glVertex3fv(verticies[5]);
	glVertex3fv(verticies[1]);
	glEnd();


	
}