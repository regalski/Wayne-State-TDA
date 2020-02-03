Make a Vineyard
================

Pre-reqs
========

``` r
library('rgl')
library('TDA')
library('pracma')
library('rlist')
library('tidyverse')
#setwd('~/fmri_tda/how_to_make_vineyard/') #Change this to where you unzipped how_to_make_a_vineyard.zip
source('calc_vineyard.R')
source('plot_vineyard.R')
source('helper.R')
source('brain_plots.R')
source('vinehunter.R')
```

First we will produce the same vineyard (with less time slices) as seen in the paper from pre-calculated data.
==============================================================================================================

The function *read\_in\_data* reads in precalculated vineyard data from a specified directory.

``` r
data<-read_in_data(dir_names = c('./vine_0.5_1/', '2512rdACC_masked_time_', '_patient1.rds'))
acc<- read.csv('acc.csv',header = FALSE)[,1:3]
```

The function *vineyard* creates the vineyard.

``` r
vineyard(data,range=c(1,10))
```

<script>/*
* Copyright (C) 2009 Apple Inc. All Rights Reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
* 1. Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
* PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
* Copyright (2016) Duncan Murdoch - fixed CanvasMatrix4.ortho,
* cleaned up.
*/
/*
CanvasMatrix4 class
This class implements a 4x4 matrix. It has functions which
duplicate the functionality of the OpenGL matrix stack and
glut functions.
IDL:
[
Constructor(in CanvasMatrix4 matrix),           // copy passed matrix into new CanvasMatrix4
Constructor(in sequence<float> array)           // create new CanvasMatrix4 with 16 floats (row major)
Constructor()                                   // create new CanvasMatrix4 with identity matrix
]
interface CanvasMatrix4 {
attribute float m11;
attribute float m12;
attribute float m13;
attribute float m14;
attribute float m21;
attribute float m22;
attribute float m23;
attribute float m24;
attribute float m31;
attribute float m32;
attribute float m33;
attribute float m34;
attribute float m41;
attribute float m42;
attribute float m43;
attribute float m44;
void load(in CanvasMatrix4 matrix);                 // copy the values from the passed matrix
void load(in sequence<float> array);                // copy 16 floats into the matrix
sequence<float> getAsArray();                       // return the matrix as an array of 16 floats
WebGLFloatArray getAsCanvasFloatArray();           // return the matrix as a WebGLFloatArray with 16 values
void makeIdentity();                                // replace the matrix with identity
void transpose();                                   // replace the matrix with its transpose
void invert();                                      // replace the matrix with its inverse
void translate(in float x, in float y, in float z); // multiply the matrix by passed translation values on the right
void scale(in float x, in float y, in float z);     // multiply the matrix by passed scale values on the right
void rotate(in float angle,                         // multiply the matrix by passed rotation values on the right
in float x, in float y, in float z);    // (angle is in degrees)
void multRight(in CanvasMatrix matrix);             // multiply the matrix by the passed matrix on the right
void multLeft(in CanvasMatrix matrix);              // multiply the matrix by the passed matrix on the left
void ortho(in float left, in float right,           // multiply the matrix by the passed ortho values on the right
in float bottom, in float top,
in float near, in float far);
void frustum(in float left, in float right,         // multiply the matrix by the passed frustum values on the right
in float bottom, in float top,
in float near, in float far);
void perspective(in float fovy, in float aspect,    // multiply the matrix by the passed perspective values on the right
in float zNear, in float zFar);
void lookat(in float eyex, in float eyey, in float eyez,    // multiply the matrix by the passed lookat
in float ctrx, in float ctry, in float ctrz,    // values on the right
in float upx, in float upy, in float upz);
}
*/
CanvasMatrix4 = function(m)
{
if (typeof m == 'object') {
if ("length" in m && m.length >= 16) {
this.load(m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9], m[10], m[11], m[12], m[13], m[14], m[15]);
return;
}
else if (m instanceof CanvasMatrix4) {
this.load(m);
return;
}
}
this.makeIdentity();
};
CanvasMatrix4.prototype.load = function()
{
if (arguments.length == 1 && typeof arguments[0] == 'object') {
var matrix = arguments[0];
if ("length" in matrix && matrix.length == 16) {
this.m11 = matrix[0];
this.m12 = matrix[1];
this.m13 = matrix[2];
this.m14 = matrix[3];
this.m21 = matrix[4];
this.m22 = matrix[5];
this.m23 = matrix[6];
this.m24 = matrix[7];
this.m31 = matrix[8];
this.m32 = matrix[9];
this.m33 = matrix[10];
this.m34 = matrix[11];
this.m41 = matrix[12];
this.m42 = matrix[13];
this.m43 = matrix[14];
this.m44 = matrix[15];
return;
}
if (arguments[0] instanceof CanvasMatrix4) {
this.m11 = matrix.m11;
this.m12 = matrix.m12;
this.m13 = matrix.m13;
this.m14 = matrix.m14;
this.m21 = matrix.m21;
this.m22 = matrix.m22;
this.m23 = matrix.m23;
this.m24 = matrix.m24;
this.m31 = matrix.m31;
this.m32 = matrix.m32;
this.m33 = matrix.m33;
this.m34 = matrix.m34;
this.m41 = matrix.m41;
this.m42 = matrix.m42;
this.m43 = matrix.m43;
this.m44 = matrix.m44;
return;
}
}
this.makeIdentity();
};
CanvasMatrix4.prototype.getAsArray = function()
{
return [
this.m11, this.m12, this.m13, this.m14,
this.m21, this.m22, this.m23, this.m24,
this.m31, this.m32, this.m33, this.m34,
this.m41, this.m42, this.m43, this.m44
];
};
CanvasMatrix4.prototype.getAsWebGLFloatArray = function()
{
return new WebGLFloatArray(this.getAsArray());
};
CanvasMatrix4.prototype.makeIdentity = function()
{
this.m11 = 1;
this.m12 = 0;
this.m13 = 0;
this.m14 = 0;
this.m21 = 0;
this.m22 = 1;
this.m23 = 0;
this.m24 = 0;
this.m31 = 0;
this.m32 = 0;
this.m33 = 1;
this.m34 = 0;
this.m41 = 0;
this.m42 = 0;
this.m43 = 0;
this.m44 = 1;
};
CanvasMatrix4.prototype.transpose = function()
{
var tmp = this.m12;
this.m12 = this.m21;
this.m21 = tmp;
tmp = this.m13;
this.m13 = this.m31;
this.m31 = tmp;
tmp = this.m14;
this.m14 = this.m41;
this.m41 = tmp;
tmp = this.m23;
this.m23 = this.m32;
this.m32 = tmp;
tmp = this.m24;
this.m24 = this.m42;
this.m42 = tmp;
tmp = this.m34;
this.m34 = this.m43;
this.m43 = tmp;
};
CanvasMatrix4.prototype.invert = function()
{
// Calculate the 4x4 determinant
// If the determinant is zero,
// then the inverse matrix is not unique.
var det = this._determinant4x4();
if (Math.abs(det) < 1e-8)
return null;
this._makeAdjoint();
// Scale the adjoint matrix to get the inverse
this.m11 /= det;
this.m12 /= det;
this.m13 /= det;
this.m14 /= det;
this.m21 /= det;
this.m22 /= det;
this.m23 /= det;
this.m24 /= det;
this.m31 /= det;
this.m32 /= det;
this.m33 /= det;
this.m34 /= det;
this.m41 /= det;
this.m42 /= det;
this.m43 /= det;
this.m44 /= det;
};
CanvasMatrix4.prototype.translate = function(x,y,z)
{
if (x === undefined)
x = 0;
if (y === undefined)
y = 0;
if (z === undefined)
z = 0;
var matrix = new CanvasMatrix4();
matrix.m41 = x;
matrix.m42 = y;
matrix.m43 = z;
this.multRight(matrix);
};
CanvasMatrix4.prototype.scale = function(x,y,z)
{
if (x === undefined)
x = 1;
if (z === undefined) {
if (y === undefined) {
y = x;
z = x;
}
else
z = 1;
}
else if (y === undefined)
y = x;
var matrix = new CanvasMatrix4();
matrix.m11 = x;
matrix.m22 = y;
matrix.m33 = z;
this.multRight(matrix);
};
CanvasMatrix4.prototype.rotate = function(angle,x,y,z)
{
// angles are in degrees. Switch to radians
angle = angle / 180 * Math.PI;
angle /= 2;
var sinA = Math.sin(angle);
var cosA = Math.cos(angle);
var sinA2 = sinA * sinA;
// normalize
var length = Math.sqrt(x * x + y * y + z * z);
if (length === 0) {
// bad vector, just use something reasonable
x = 0;
y = 0;
z = 1;
} else if (length != 1) {
x /= length;
y /= length;
z /= length;
}
var mat = new CanvasMatrix4();
// optimize case where axis is along major axis
if (x == 1 && y === 0 && z === 0) {
mat.m11 = 1;
mat.m12 = 0;
mat.m13 = 0;
mat.m21 = 0;
mat.m22 = 1 - 2 * sinA2;
mat.m23 = 2 * sinA * cosA;
mat.m31 = 0;
mat.m32 = -2 * sinA * cosA;
mat.m33 = 1 - 2 * sinA2;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
} else if (x === 0 && y == 1 && z === 0) {
mat.m11 = 1 - 2 * sinA2;
mat.m12 = 0;
mat.m13 = -2 * sinA * cosA;
mat.m21 = 0;
mat.m22 = 1;
mat.m23 = 0;
mat.m31 = 2 * sinA * cosA;
mat.m32 = 0;
mat.m33 = 1 - 2 * sinA2;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
} else if (x === 0 && y === 0 && z == 1) {
mat.m11 = 1 - 2 * sinA2;
mat.m12 = 2 * sinA * cosA;
mat.m13 = 0;
mat.m21 = -2 * sinA * cosA;
mat.m22 = 1 - 2 * sinA2;
mat.m23 = 0;
mat.m31 = 0;
mat.m32 = 0;
mat.m33 = 1;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
} else {
var x2 = x*x;
var y2 = y*y;
var z2 = z*z;
mat.m11 = 1 - 2 * (y2 + z2) * sinA2;
mat.m12 = 2 * (x * y * sinA2 + z * sinA * cosA);
mat.m13 = 2 * (x * z * sinA2 - y * sinA * cosA);
mat.m21 = 2 * (y * x * sinA2 - z * sinA * cosA);
mat.m22 = 1 - 2 * (z2 + x2) * sinA2;
mat.m23 = 2 * (y * z * sinA2 + x * sinA * cosA);
mat.m31 = 2 * (z * x * sinA2 + y * sinA * cosA);
mat.m32 = 2 * (z * y * sinA2 - x * sinA * cosA);
mat.m33 = 1 - 2 * (x2 + y2) * sinA2;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
}
this.multRight(mat);
};
CanvasMatrix4.prototype.multRight = function(mat)
{
var m11 = (this.m11 * mat.m11 + this.m12 * mat.m21 +
this.m13 * mat.m31 + this.m14 * mat.m41);
var m12 = (this.m11 * mat.m12 + this.m12 * mat.m22 +
this.m13 * mat.m32 + this.m14 * mat.m42);
var m13 = (this.m11 * mat.m13 + this.m12 * mat.m23 +
this.m13 * mat.m33 + this.m14 * mat.m43);
var m14 = (this.m11 * mat.m14 + this.m12 * mat.m24 +
this.m13 * mat.m34 + this.m14 * mat.m44);
var m21 = (this.m21 * mat.m11 + this.m22 * mat.m21 +
this.m23 * mat.m31 + this.m24 * mat.m41);
var m22 = (this.m21 * mat.m12 + this.m22 * mat.m22 +
this.m23 * mat.m32 + this.m24 * mat.m42);
var m23 = (this.m21 * mat.m13 + this.m22 * mat.m23 +
this.m23 * mat.m33 + this.m24 * mat.m43);
var m24 = (this.m21 * mat.m14 + this.m22 * mat.m24 +
this.m23 * mat.m34 + this.m24 * mat.m44);
var m31 = (this.m31 * mat.m11 + this.m32 * mat.m21 +
this.m33 * mat.m31 + this.m34 * mat.m41);
var m32 = (this.m31 * mat.m12 + this.m32 * mat.m22 +
this.m33 * mat.m32 + this.m34 * mat.m42);
var m33 = (this.m31 * mat.m13 + this.m32 * mat.m23 +
this.m33 * mat.m33 + this.m34 * mat.m43);
var m34 = (this.m31 * mat.m14 + this.m32 * mat.m24 +
this.m33 * mat.m34 + this.m34 * mat.m44);
var m41 = (this.m41 * mat.m11 + this.m42 * mat.m21 +
this.m43 * mat.m31 + this.m44 * mat.m41);
var m42 = (this.m41 * mat.m12 + this.m42 * mat.m22 +
this.m43 * mat.m32 + this.m44 * mat.m42);
var m43 = (this.m41 * mat.m13 + this.m42 * mat.m23 +
this.m43 * mat.m33 + this.m44 * mat.m43);
var m44 = (this.m41 * mat.m14 + this.m42 * mat.m24 +
this.m43 * mat.m34 + this.m44 * mat.m44);
this.m11 = m11;
this.m12 = m12;
this.m13 = m13;
this.m14 = m14;
this.m21 = m21;
this.m22 = m22;
this.m23 = m23;
this.m24 = m24;
this.m31 = m31;
this.m32 = m32;
this.m33 = m33;
this.m34 = m34;
this.m41 = m41;
this.m42 = m42;
this.m43 = m43;
this.m44 = m44;
};
CanvasMatrix4.prototype.multLeft = function(mat)
{
var m11 = (mat.m11 * this.m11 + mat.m12 * this.m21 +
mat.m13 * this.m31 + mat.m14 * this.m41);
var m12 = (mat.m11 * this.m12 + mat.m12 * this.m22 +
mat.m13 * this.m32 + mat.m14 * this.m42);
var m13 = (mat.m11 * this.m13 + mat.m12 * this.m23 +
mat.m13 * this.m33 + mat.m14 * this.m43);
var m14 = (mat.m11 * this.m14 + mat.m12 * this.m24 +
mat.m13 * this.m34 + mat.m14 * this.m44);
var m21 = (mat.m21 * this.m11 + mat.m22 * this.m21 +
mat.m23 * this.m31 + mat.m24 * this.m41);
var m22 = (mat.m21 * this.m12 + mat.m22 * this.m22 +
mat.m23 * this.m32 + mat.m24 * this.m42);
var m23 = (mat.m21 * this.m13 + mat.m22 * this.m23 +
mat.m23 * this.m33 + mat.m24 * this.m43);
var m24 = (mat.m21 * this.m14 + mat.m22 * this.m24 +
mat.m23 * this.m34 + mat.m24 * this.m44);
var m31 = (mat.m31 * this.m11 + mat.m32 * this.m21 +
mat.m33 * this.m31 + mat.m34 * this.m41);
var m32 = (mat.m31 * this.m12 + mat.m32 * this.m22 +
mat.m33 * this.m32 + mat.m34 * this.m42);
var m33 = (mat.m31 * this.m13 + mat.m32 * this.m23 +
mat.m33 * this.m33 + mat.m34 * this.m43);
var m34 = (mat.m31 * this.m14 + mat.m32 * this.m24 +
mat.m33 * this.m34 + mat.m34 * this.m44);
var m41 = (mat.m41 * this.m11 + mat.m42 * this.m21 +
mat.m43 * this.m31 + mat.m44 * this.m41);
var m42 = (mat.m41 * this.m12 + mat.m42 * this.m22 +
mat.m43 * this.m32 + mat.m44 * this.m42);
var m43 = (mat.m41 * this.m13 + mat.m42 * this.m23 +
mat.m43 * this.m33 + mat.m44 * this.m43);
var m44 = (mat.m41 * this.m14 + mat.m42 * this.m24 +
mat.m43 * this.m34 + mat.m44 * this.m44);
this.m11 = m11;
this.m12 = m12;
this.m13 = m13;
this.m14 = m14;
this.m21 = m21;
this.m22 = m22;
this.m23 = m23;
this.m24 = m24;
this.m31 = m31;
this.m32 = m32;
this.m33 = m33;
this.m34 = m34;
this.m41 = m41;
this.m42 = m42;
this.m43 = m43;
this.m44 = m44;
};
CanvasMatrix4.prototype.ortho = function(left, right, bottom, top, near, far)
{
var tx = (left + right) / (left - right);
var ty = (top + bottom) / (bottom - top);
var tz = (far + near) / (near - far);
var matrix = new CanvasMatrix4();
matrix.m11 = 2 / (right - left);
matrix.m12 = 0;
matrix.m13 = 0;
matrix.m14 = 0;
matrix.m21 = 0;
matrix.m22 = 2 / (top - bottom);
matrix.m23 = 0;
matrix.m24 = 0;
matrix.m31 = 0;
matrix.m32 = 0;
matrix.m33 = -2 / (far - near);
matrix.m34 = 0;
matrix.m41 = tx;
matrix.m42 = ty;
matrix.m43 = tz;
matrix.m44 = 1;
this.multRight(matrix);
};
CanvasMatrix4.prototype.frustum = function(left, right, bottom, top, near, far)
{
var matrix = new CanvasMatrix4();
var A = (right + left) / (right - left);
var B = (top + bottom) / (top - bottom);
var C = -(far + near) / (far - near);
var D = -(2 * far * near) / (far - near);
matrix.m11 = (2 * near) / (right - left);
matrix.m12 = 0;
matrix.m13 = 0;
matrix.m14 = 0;
matrix.m21 = 0;
matrix.m22 = 2 * near / (top - bottom);
matrix.m23 = 0;
matrix.m24 = 0;
matrix.m31 = A;
matrix.m32 = B;
matrix.m33 = C;
matrix.m34 = -1;
matrix.m41 = 0;
matrix.m42 = 0;
matrix.m43 = D;
matrix.m44 = 0;
this.multRight(matrix);
};
CanvasMatrix4.prototype.perspective = function(fovy, aspect, zNear, zFar)
{
var top = Math.tan(fovy * Math.PI / 360) * zNear;
var bottom = -top;
var left = aspect * bottom;
var right = aspect * top;
this.frustum(left, right, bottom, top, zNear, zFar);
};
CanvasMatrix4.prototype.lookat = function(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz)
{
var matrix = new CanvasMatrix4();
// Make rotation matrix
// Z vector
var zx = eyex - centerx;
var zy = eyey - centery;
var zz = eyez - centerz;
var mag = Math.sqrt(zx * zx + zy * zy + zz * zz);
if (mag) {
zx /= mag;
zy /= mag;
zz /= mag;
}
// Y vector
var yx = upx;
var yy = upy;
var yz = upz;
// X vector = Y cross Z
xx =  yy * zz - yz * zy;
xy = -yx * zz + yz * zx;
xz =  yx * zy - yy * zx;
// Recompute Y = Z cross X
yx = zy * xz - zz * xy;
yy = -zx * xz + zz * xx;
yx = zx * xy - zy * xx;
// cross product gives area of parallelogram, which is < 1.0 for
// non-perpendicular unit-length vectors; so normalize x, y here
mag = Math.sqrt(xx * xx + xy * xy + xz * xz);
if (mag) {
xx /= mag;
xy /= mag;
xz /= mag;
}
mag = Math.sqrt(yx * yx + yy * yy + yz * yz);
if (mag) {
yx /= mag;
yy /= mag;
yz /= mag;
}
matrix.m11 = xx;
matrix.m12 = xy;
matrix.m13 = xz;
matrix.m14 = 0;
matrix.m21 = yx;
matrix.m22 = yy;
matrix.m23 = yz;
matrix.m24 = 0;
matrix.m31 = zx;
matrix.m32 = zy;
matrix.m33 = zz;
matrix.m34 = 0;
matrix.m41 = 0;
matrix.m42 = 0;
matrix.m43 = 0;
matrix.m44 = 1;
matrix.translate(-eyex, -eyey, -eyez);
this.multRight(matrix);
};
// Support functions
CanvasMatrix4.prototype._determinant2x2 = function(a, b, c, d)
{
return a * d - b * c;
};
CanvasMatrix4.prototype._determinant3x3 = function(a1, a2, a3, b1, b2, b3, c1, c2, c3)
{
return a1 * this._determinant2x2(b2, b3, c2, c3) -
b1 * this._determinant2x2(a2, a3, c2, c3) +
c1 * this._determinant2x2(a2, a3, b2, b3);
};
CanvasMatrix4.prototype._determinant4x4 = function()
{
var a1 = this.m11;
var b1 = this.m12;
var c1 = this.m13;
var d1 = this.m14;
var a2 = this.m21;
var b2 = this.m22;
var c2 = this.m23;
var d2 = this.m24;
var a3 = this.m31;
var b3 = this.m32;
var c3 = this.m33;
var d3 = this.m34;
var a4 = this.m41;
var b4 = this.m42;
var c4 = this.m43;
var d4 = this.m44;
return a1 * this._determinant3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
b1 * this._determinant3x3(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
c1 * this._determinant3x3(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
d1 * this._determinant3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4);
};
CanvasMatrix4.prototype._makeAdjoint = function()
{
var a1 = this.m11;
var b1 = this.m12;
var c1 = this.m13;
var d1 = this.m14;
var a2 = this.m21;
var b2 = this.m22;
var c2 = this.m23;
var d2 = this.m24;
var a3 = this.m31;
var b3 = this.m32;
var c3 = this.m33;
var d3 = this.m34;
var a4 = this.m41;
var b4 = this.m42;
var c4 = this.m43;
var d4 = this.m44;
// Row column labeling reversed since we transpose rows & columns
this.m11  =   this._determinant3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4);
this.m21  = - this._determinant3x3(a2, a3, a4, c2, c3, c4, d2, d3, d4);
this.m31  =   this._determinant3x3(a2, a3, a4, b2, b3, b4, d2, d3, d4);
this.m41  = - this._determinant3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4);
this.m12  = - this._determinant3x3(b1, b3, b4, c1, c3, c4, d1, d3, d4);
this.m22  =   this._determinant3x3(a1, a3, a4, c1, c3, c4, d1, d3, d4);
this.m32  = - this._determinant3x3(a1, a3, a4, b1, b3, b4, d1, d3, d4);
this.m42  =   this._determinant3x3(a1, a3, a4, b1, b3, b4, c1, c3, c4);
this.m13  =   this._determinant3x3(b1, b2, b4, c1, c2, c4, d1, d2, d4);
this.m23  = - this._determinant3x3(a1, a2, a4, c1, c2, c4, d1, d2, d4);
this.m33  =   this._determinant3x3(a1, a2, a4, b1, b2, b4, d1, d2, d4);
this.m43  = - this._determinant3x3(a1, a2, a4, b1, b2, b4, c1, c2, c4);
this.m14  = - this._determinant3x3(b1, b2, b3, c1, c2, c3, d1, d2, d3);
this.m24  =   this._determinant3x3(a1, a2, a3, c1, c2, c3, d1, d2, d3);
this.m34  = - this._determinant3x3(a1, a2, a3, b1, b2, b3, d1, d2, d3);
this.m44  =   this._determinant3x3(a1, a2, a3, b1, b2, b3, c1, c2, c3);
};</script>
<script>// To generate the help pages for this library, use
// jsdoc --destination ../../../doc/rglwidgetClass --template ~/node_modules/jsdoc-baseline rglClass.src.js
// To validate, use
// setwd(".../inst/htmlwidgets/lib/rglClass")
// hints <- js::jshint(readLines("rglClass.src.js"))
// hints[, c("line", "reason")]
/**
* The class of an rgl widget
* @class
*/
rglwidgetClass = function() {
this.canvas = null;
this.userMatrix = new CanvasMatrix4();
this.types = [];
this.prMatrix = new CanvasMatrix4();
this.mvMatrix = new CanvasMatrix4();
this.vp = null;
this.prmvMatrix = null;
this.origs = null;
this.gl = null;
this.scene = null;
this.select = {state: "inactive", subscene: null, region: {p1: {x:0, y:0}, p2: {x:0, y:0}}};
this.drawing = false;
};
/**
* Multiply matrix by vector
* @returns {number[]}
* @param M {number[][]} Left operand
* @param v {number[]} Right operand
*/
rglwidgetClass.prototype.multMV = function(M, v) {
return [ M.m11 * v[0] + M.m12 * v[1] + M.m13 * v[2] + M.m14 * v[3],
M.m21 * v[0] + M.m22 * v[1] + M.m23 * v[2] + M.m24 * v[3],
M.m31 * v[0] + M.m32 * v[1] + M.m33 * v[2] + M.m34 * v[3],
M.m41 * v[0] + M.m42 * v[1] + M.m43 * v[2] + M.m44 * v[3]
];
};
/**
* Multiply row vector by Matrix
* @returns {number[]}
* @param v {number[]} left operand
* @param M {number[][]} right operand
*/
rglwidgetClass.prototype.multVM = function(v, M) {
return [ M.m11 * v[0] + M.m21 * v[1] + M.m31 * v[2] + M.m41 * v[3],
M.m12 * v[0] + M.m22 * v[1] + M.m32 * v[2] + M.m42 * v[3],
M.m13 * v[0] + M.m23 * v[1] + M.m33 * v[2] + M.m43 * v[3],
M.m14 * v[0] + M.m24 * v[1] + M.m34 * v[2] + M.m44 * v[3]
];
};
/**
* Euclidean length of a vector
* @returns {number}
* @param v {number[]}
*/
rglwidgetClass.prototype.vlen = function(v) {
return Math.sqrt(this.dotprod(v, v));
};
/**
* Dot product of two vectors
* @instance rglwidgetClass
* @returns {number}
* @param a {number[]}
* @param b {number[]}
*/
rglwidgetClass.prototype.dotprod = function(a, b) {
return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
};
/**
* Cross product of two vectors
* @returns {number[]}
* @param a {number[]}
* @param b {number[]}
*/
rglwidgetClass.prototype.xprod = function(a, b) {
return [a[1]*b[2] - a[2]*b[1],
a[2]*b[0] - a[0]*b[2],
a[0]*b[1] - a[1]*b[0]];
};
/**
* Bind vectors or matrices by columns
* @returns {number[][]}
* @param a {number[]|number[][]}
* @param b {number[]|number[][]}
*/
rglwidgetClass.prototype.cbind = function(a, b) {
if (b.length < a.length)
b = this.repeatToLen(b, a.length);
else if (a.length < b.length)
a = this.repeatToLen(a, b.length);
return a.map(function(currentValue, index, array) {
return currentValue.concat(b[index]);
});
};
/**
* Swap elements
* @returns {any[]}
* @param a {any[]}
* @param i {number} Element to swap
* @param j {number} Other element to swap
*/
rglwidgetClass.prototype.swap = function(a, i, j) {
var temp = a[i];
a[i] = a[j];
a[j] = temp;
};
/**
* Flatten a matrix into a vector
* @returns {any[]}
* @param a {any[][]}
*/
rglwidgetClass.prototype.flatten = function(arr, result) {
var value;
if (typeof result === "undefined") result = [];
for (var i = 0, length = arr.length; i < length; i++) {
value = arr[i];
if (Array.isArray(value)) {
this.flatten(value, result);
} else {
result.push(value);
}
}
return result;
};
/**
* set element of 1d or 2d array as if it was flattened.
* Column major, zero based!
* @returns {any[]|any[][]}
* @param {any[]|any[][]} a - array
* @param {number} i - element
* @param {any} value
*/
rglwidgetClass.prototype.setElement = function(a, i, value) {
if (Array.isArray(a[0])) {
var dim = a.length,
col = Math.floor(i/dim),
row = i % dim;
a[row][col] = value;
} else {
a[i] = value;
}
};
/**
* Transpose an array
* @returns {any[][]}
* @param {any[][]} a
*/
rglwidgetClass.prototype.transpose = function(a) {
var newArray = [],
n = a.length,
m = a[0].length,
i;
for(i = 0; i < m; i++){
newArray.push([]);
}
for(i = 0; i < n; i++){
for(var j = 0; j < m; j++){
newArray[j].push(a[i][j]);
}
}
return newArray;
};
/**
* Calculate sum of squares of a numeric vector
* @returns {number}
* @param {number[]} x
*/
rglwidgetClass.prototype.sumsq = function(x) {
var result = 0, i;
for (i=0; i < x.length; i++)
result += x[i]*x[i];
return result;
};
/**
* Convert a matrix to a CanvasMatrix4
* @returns {CanvasMatrix4}
* @param {number[][]|number[]} mat
*/
rglwidgetClass.prototype.toCanvasMatrix4 = function(mat) {
if (mat instanceof CanvasMatrix4)
return mat;
var result = new CanvasMatrix4();
mat = this.flatten(this.transpose(mat));
result.load(mat);
return result;
};
/**
* Convert an R-style numeric colour string to an rgb vector
* @returns {number[]}
* @param {string} s
*/
rglwidgetClass.prototype.stringToRgb = function(s) {
s = s.replace("#", "");
var bigint = parseInt(s, 16);
return [((bigint >> 16) & 255)/255,
((bigint >> 8) & 255)/255,
(bigint & 255)/255];
};
/**
* Take a component-by-component product of two 3 vectors
* @returns {number[]}
* @param {number[]} x
* @param {number[]} y
*/
rglwidgetClass.prototype.componentProduct = function(x, y) {
if (typeof y === "undefined") {
this.alertOnce("Bad arg to componentProduct");
}
var result = new Float32Array(3), i;
for (i = 0; i<3; i++)
result[i] = x[i]*y[i];
return result;
};
/**
* Get next higher power of two
* @returns { number }
* @param { number } value - input value
*/
rglwidgetClass.prototype.getPowerOfTwo = function(value) {
var pow = 1;
while(pow<value) {
pow *= 2;
}
return pow;
};
/**
* Unique entries
* @returns { any[] }
* @param { any[] } arr - An array
*/
rglwidgetClass.prototype.unique = function(arr) {
arr = [].concat(arr);
return arr.filter(function(value, index, self) {
return self.indexOf(value) === index;
});
};
/**
* Shallow compare of arrays
* @returns { boolean }
* @param { any[] } a - An array
* @param { any[] } b - Another array
*/
rglwidgetClass.prototype.equalArrays = function(a, b) {
return a === b || (a && b &&
a.length === b.length &&
a.every(function(v, i) {return v === b[i];}));
};
/**
* Repeat an array to a desired length
* @returns {any[]}
* @param {any | any[]} arr The input array
* @param {number} len The desired output length
*/
rglwidgetClass.prototype.repeatToLen = function(arr, len) {
arr = [].concat(arr);
while (arr.length < len/2)
arr = arr.concat(arr);
return arr.concat(arr.slice(0, len - arr.length));
};
/**
* Give a single alert message, not to be repeated.
* @param {string} msg  The message to give.
*/
rglwidgetClass.prototype.alertOnce = function(msg) {
if (typeof this.alerted !== "undefined")
return;
this.alerted = true;
alert(msg);
};
rglwidgetClass.prototype.f_is_lit = 1;
rglwidgetClass.prototype.f_is_smooth = 2;
rglwidgetClass.prototype.f_has_texture = 4;
rglwidgetClass.prototype.f_depth_sort = 8;
rglwidgetClass.prototype.f_fixed_quads = 16;
rglwidgetClass.prototype.f_is_transparent = 32;
rglwidgetClass.prototype.f_is_lines = 64;
rglwidgetClass.prototype.f_sprites_3d = 128;
rglwidgetClass.prototype.f_sprite_3d = 256;
rglwidgetClass.prototype.f_is_subscene = 512;
rglwidgetClass.prototype.f_is_clipplanes = 1024;
rglwidgetClass.prototype.f_fixed_size = 2048;
rglwidgetClass.prototype.f_is_points = 4096;
rglwidgetClass.prototype.f_is_twosided = 8192;
rglwidgetClass.prototype.f_fat_lines = 16384;
rglwidgetClass.prototype.f_is_brush = 32768;
/**
* Which list does a particular id come from?
* @returns { string }
* @param {number} id The id to look up.
*/
rglwidgetClass.prototype.whichList = function(id) {
var obj = this.getObj(id),
flags = obj.flags;
if (obj.type === "light")
return "lights";
if (flags & this.f_is_subscene)
return "subscenes";
if (flags & this.f_is_clipplanes)
return "clipplanes";
if (flags & this.f_is_transparent)
return "transparent";
return "opaque";
};
/**
* Get an object by id number.
* @returns { Object }
* @param {number} id
*/
rglwidgetClass.prototype.getObj = function(id) {
if (typeof id !== "number") {
this.alertOnce("getObj id is "+typeof id);
}
return this.scene.objects[id];
};
/**
* Get ids of a particular type from a subscene or the whole scene
* @returns { number[] }
* @param {string} type What type of object?
* @param {number} subscene  Which subscene?  If not given, find in the whole scene
*/
rglwidgetClass.prototype.getIdsByType = function(type, subscene) {
var
result = [], i, self = this;
if (typeof subscene === "undefined") {
Object.keys(this.scene.objects).forEach(
function(key) {
key = parseInt(key, 10);
if (self.getObj(key).type === type)
result.push(key);
});
} else {
ids = this.getObj(subscene).objects;
for (i=0; i < ids.length; i++) {
if (this.getObj(ids[i]).type === type) {
result.push(ids[i]);
}
}
}
return result;
};
/**
* Get a particular material property for an id
* @returns { any }
* @param {number} id  Which object?
* @param {string} property Which material property?
*/
rglwidgetClass.prototype.getMaterial = function(id, property) {
var obj = this.getObj(id),
mat = obj.material[property];
if (typeof mat === "undefined")
mat = this.scene.material[property];
return mat;
};
/**
* Is a particular id in a subscene?
* @returns { boolean }
* @param {number} id Which id?
* @param {number} subscene Which subscene id?
*/
rglwidgetClass.prototype.inSubscene = function(id, subscene) {
return this.getObj(subscene).objects.indexOf(id) > -1;
};
/**
* Add an id to a subscene.
* @param {number} id Which id?
* @param {number} subscene Which subscene id?
*/
rglwidgetClass.prototype.addToSubscene = function(id, subscene) {
var thelist,
thesub = this.getObj(subscene),
ids = [id],
obj = this.getObj(id), i;
if (typeof obj != "undefined" && typeof (obj.newIds) !== "undefined") {
ids = ids.concat(obj.newIds);
}
thesub.objects = [].concat(thesub.objects);
for (i = 0; i < ids.length; i++) {
id = ids[i];
if (thesub.objects.indexOf(id) == -1) {
thelist = this.whichList(id);
thesub.objects.push(id);
thesub[thelist].push(id);
}
}
};
/**
* Delete an id from a subscene
* @param { number } id - the id to add
* @param { number } subscene - the id of the subscene
*/
rglwidgetClass.prototype.delFromSubscene = function(id, subscene) {
var thelist,
thesub = this.getObj(subscene),
obj = this.getObj(id),
ids = [id], i;
if (typeof obj !== "undefined" && typeof (obj.newIds) !== "undefined")
ids = ids.concat(obj.newIds);
thesub.objects = [].concat(thesub.objects); // It might be a scalar
for (j=0; j<ids.length;j++) {
id = ids[j];
i = thesub.objects.indexOf(id);
if (i > -1) {
thesub.objects.splice(i, 1);
thelist = this.whichList(id);
i = thesub[thelist].indexOf(id);
thesub[thelist].splice(i, 1);
}
}
};
/**
* Set the ids in a subscene
* @param { number[] } ids - the ids to set
* @param { number } subsceneid - the id of the subscene
*/
rglwidgetClass.prototype.setSubsceneEntries = function(ids, subsceneid) {
var sub = this.getObj(subsceneid);
sub.objects = ids;
this.initSubscene(subsceneid);
};
/**
* Get the ids in a subscene
* @returns {number[]}
* @param { number } subscene - the id of the subscene
*/
rglwidgetClass.prototype.getSubsceneEntries = function(subscene) {
return this.getObj(subscene).objects;
};
/**
* Get the ids of the subscenes within a subscene
* @returns { number[] }
* @param { number } subscene - the id of the subscene
*/
rglwidgetClass.prototype.getChildSubscenes = function(subscene) {
return this.getObj(subscene).subscenes;
};
/**
* Start drawing
* @returns { boolean } Previous state
*/
rglwidgetClass.prototype.startDrawing = function() {
var value = this.drawing;
this.drawing = true;
return value;
};
/**
* Stop drawing and check for context loss
* @param { boolean } saved - Previous state
*/
rglwidgetClass.prototype.stopDrawing = function(saved) {
this.drawing = saved;
if (!saved && this.gl && this.gl.isContextLost())
this.restartCanvas();
};
/**
* Generate the vertex shader for an object
* @returns {string}
* @param { number } id - Id of object
*/
rglwidgetClass.prototype.getVertexShader = function(id) {
var obj = this.getObj(id),
userShader = obj.userVertexShader,
flags = obj.flags,
type = obj.type,
is_lit = flags & this.f_is_lit,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
sprites_3d = flags & this.f_sprites_3d,
sprite_3d = flags & this.f_sprite_3d,
nclipplanes = this.countClipplanes(),
fixed_size = flags & this.f_fixed_size,
is_points = flags & this.f_is_points,
is_twosided = flags & this.f_is_twosided,
fat_lines = flags & this.f_fat_lines,
is_brush = flags & this.f_is_brush,
result;
if (type === "clipplanes" || sprites_3d) return;
if (typeof userShader !== "undefined") return userShader;
result = "  /* ****** "+type+" object "+id+" vertex shader ****** */\n"+
"  attribute vec3 aPos;\n"+
"  attribute vec4 aCol;\n"+
" uniform mat4 mvMatrix;\n"+
" uniform mat4 prMatrix;\n"+
" varying vec4 vCol;\n"+
" varying vec4 vPosition;\n";
if ((is_lit && !fixed_quads && !is_brush) || sprite_3d)
result = result + "  attribute vec3 aNorm;\n"+
" uniform mat4 normMatrix;\n"+
" varying vec3 vNormal;\n";
if (has_texture || type === "text")
result = result + " attribute vec2 aTexcoord;\n"+
" varying vec2 vTexcoord;\n";
if (fixed_size)
result = result + "  uniform vec2 textScale;\n";
if (fixed_quads)
result = result + "  attribute vec2 aOfs;\n";
else if (sprite_3d)
result = result + "  uniform vec3 uOrig;\n"+
"  uniform float uSize;\n"+
"  uniform mat4 usermat;\n";
if (is_twosided)
result = result + "  attribute vec3 aPos1;\n"+
"  attribute vec3 aPos2;\n"+
"  varying float normz;\n";
if (fat_lines) {
result = result +   "  attribute vec3 aNext;\n"+
"  attribute vec2 aPoint;\n"+
"  varying vec2 vPoint;\n"+
"  varying float vLength;\n"+
"  uniform float uAspect;\n"+
"  uniform float uLwd;\n";
}
result = result + "  void main(void) {\n";
if ((nclipplanes || (!fixed_quads && !sprite_3d)) && !is_brush)
result = result + "    vPosition = mvMatrix * vec4(aPos, 1.);\n";
if (!fixed_quads && !sprite_3d && !is_brush)
result = result + "    gl_Position = prMatrix * vPosition;\n";
if (is_points) {
var size = this.getMaterial(id, "size");
result = result + "    gl_PointSize = "+size.toFixed(1)+";\n";
}
result = result + "    vCol = aCol;\n";
if (is_lit && !fixed_quads && !sprite_3d && !is_brush)
result = result + "    vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);\n";
if (has_texture || type == "text")
result = result + "    vTexcoord = aTexcoord;\n";
if (fixed_size)
result = result + "    vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n"+
"   pos = pos/pos.w;\n"+
"   gl_Position = pos + vec4(aOfs*textScale, 0.,0.);\n";
if (type == "sprites" && !fixed_size)
result = result + "    vec4 pos = mvMatrix * vec4(aPos, 1.);\n"+
"   pos = pos/pos.w + vec4(aOfs, 0., 0.);\n"+
"   gl_Position = prMatrix*pos;\n";
if (sprite_3d)
result = result + "   vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);\n"+
"   vec4 pos = mvMatrix * vec4(uOrig, 1.);\n"+
"   vPosition = pos/pos.w + vec4(uSize*(vec4(aPos, 1.)*usermat).xyz,0.);\n"+
"   gl_Position = prMatrix * vPosition;\n";
if (is_twosided)
result = result + "   vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n"+
"   pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n"+
"   vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n"+
"   pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n"+
"   normz = pos1.x*pos2.y - pos1.y*pos2.x;\n";
if (fat_lines) 
/* This code was inspired by Matt Deslauriers' code in https://mattdesl.svbtle.com/drawing-lines-is-hard */
result = result + "   vec2 aspectVec = vec2(uAspect, 1.0);\n"+
"   mat4 projViewModel = prMatrix * mvMatrix;\n"+
"   vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n"+
"   currentProjected = currentProjected/currentProjected.w;\n"+
"   vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n"+
"   vec2 currentScreen = currentProjected.xy * aspectVec;\n"+
"   vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n"+
"   float len = uLwd;\n"+
"   vec2 dir = vec2(1.0, 0.0);\n"+
"   vPoint = aPoint;\n"+
"   vLength = length(nextScreen - currentScreen)/2.0;\n"+
"   vLength = vLength/(vLength + len);\n"+
"   if (vLength > 0.0) {\n"+
"     dir = normalize(nextScreen - currentScreen);\n"+
"   }\n"+
"   vec2 normal = vec2(-dir.y, dir.x);\n"+
"   dir.x /= uAspect;\n"+
"   normal.x /= uAspect;\n"+
"   vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n"+
"   gl_Position = currentProjected + offset;\n";
if (is_brush)
result = result + "   gl_Position = vec4(aPos, 1.);\n";
result = result + "  }\n";
// console.log(result);
return result;
};
/**
* Generate the fragment shader for an object
* @returns {string}
* @param { number } id - Id of object
*/
rglwidgetClass.prototype.getFragmentShader = function(id) {
var obj = this.getObj(id),
userShader = obj.userFragmentShader,
flags = obj.flags,
type = obj.type,
is_lit = flags & this.f_is_lit,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
sprites_3d = flags & this.f_sprites_3d,
is_twosided = (flags & this.f_is_twosided) > 0,
fat_lines = flags & this.f_fat_lines,
is_transparent = flags & this.f_is_transparent,
nclipplanes = this.countClipplanes(), i,
texture_format, nlights,
result;
if (type === "clipplanes" || sprites_3d) return;
if (typeof userShader !== "undefined") return userShader;
if (has_texture)
texture_format = this.getMaterial(id, "textype");
result = "/* ****** "+type+" object "+id+" fragment shader ****** */\n"+
"#ifdef GL_ES\n"+
"  precision highp float;\n"+
"#endif\n"+
"  varying vec4 vCol; // carries alpha\n"+
"  varying vec4 vPosition;\n";
if (has_texture || type === "text")
result = result + "  varying vec2 vTexcoord;\n"+
" uniform sampler2D uSampler;\n";
if (is_lit && !fixed_quads)
result = result + "  varying vec3 vNormal;\n";
for (i = 0; i < nclipplanes; i++)
result = result + "  uniform vec4 vClipplane"+i+";\n";
if (is_lit) {
nlights = this.countLights();
if (nlights)
result = result + "  uniform mat4 mvMatrix;\n";
else
is_lit = false;
}
if (is_lit) {
result = result + "   uniform vec3 emission;\n"+
"   uniform float shininess;\n";
for (i=0; i < nlights; i++) {
result = result + "   uniform vec3 ambient" + i + ";\n"+
"   uniform vec3 specular" + i +"; // light*material\n"+
"   uniform vec3 diffuse" + i + ";\n"+
"   uniform vec3 lightDir" + i + ";\n"+
"   uniform bool viewpoint" + i + ";\n"+
"   uniform bool finite" + i + ";\n";
}
}
if (is_twosided)
result = result + "   uniform bool front;\n"+
"   varying float normz;\n";
if (fat_lines)
result = result + "   varying vec2 vPoint;\n"+
"   varying float vLength;\n";
result = result + "  void main(void) {\n";
if (fat_lines) {
result = result + "    vec2 point = vPoint;\n"+
"    bool neg = point.y < 0.0;\n"+
"    point.y = neg ? "+
"      (point.y + vLength)/(1.0 - vLength) :\n"+
"     -(point.y - vLength)/(1.0 - vLength);\n";
if (is_transparent && type == "linestrip")
result = result+"    if (neg && length(point) <= 1.0) discard;\n";
result = result + "    point.y = min(point.y, 0.0);\n"+
"    if (length(point) > 1.0) discard;\n";
}
for (i=0; i < nclipplanes;i++)
result = result + "    if (dot(vPosition, vClipplane"+i+") < 0.0) discard;\n";
if (fixed_quads) {
result = result +   "    vec3 n = vec3(0., 0., 1.);\n";
} else if (is_lit) {
result = result +   "    vec3 n = normalize(vNormal);\n";
}
if (is_twosided) {
result = result +   "    if ((normz <= 0.) != front) discard;\n";
}
if (is_lit) {
result = result + "    vec3 eye = normalize(-vPosition.xyz);\n"+
"   vec3 lightdir;\n"+
"   vec4 colDiff;\n"+
"   vec3 halfVec;\n"+
"   vec4 lighteffect = vec4(emission, 0.);\n"+
"   vec3 col;\n"+
"   float nDotL;\n";
if (!fixed_quads) {
result = result +   "   n = -faceforward(n, n, eye);\n";
}
for (i=0; i < nlights; i++) {
result = result + "   colDiff = vec4(vCol.rgb * diffuse" + i + ", vCol.a);\n"+
"   lightdir = lightDir" + i + ";\n"+
"   if (!viewpoint" + i +")\n"+
"     lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n"+
"   if (!finite" + i + ") {\n"+
"     halfVec = normalize(lightdir + eye);\n"+
"   } else {\n"+
"     lightdir = normalize(lightdir - vPosition.xyz);\n"+
"     halfVec = normalize(lightdir + eye);\n"+
"   }\n"+
"    col = ambient" + i + ";\n"+
"   nDotL = dot(n, lightdir);\n"+
"   col = col + max(nDotL, 0.) * colDiff.rgb;\n"+
"   col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular" + i + ";\n"+
"   lighteffect = lighteffect + vec4(col, colDiff.a);\n";
}
} else {
result = result +   "   vec4 colDiff = vCol;\n"+
"    vec4 lighteffect = colDiff;\n";
}
if (type === "text")
result = result +   "    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n";
if (has_texture) {
result = result + {
rgb:            "   vec4 textureColor = lighteffect*vec4(texture2D(uSampler, vTexcoord).rgb, 1.);\n",
rgba:           "   vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n",
alpha:          "   vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
"   float luminance = dot(vec3(1.,1.,1.), textureColor.rgb)/3.;\n"+
"   textureColor =  vec4(lighteffect.rgb, lighteffect.a*luminance);\n",
luminance:      "   vec4 textureColor = vec4(lighteffect.rgb*dot(texture2D(uSampler, vTexcoord).rgb, vec3(1.,1.,1.))/3., lighteffect.a);\n",
"luminance.alpha":"    vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
"   float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n"+
"   textureColor = vec4(lighteffect.rgb*luminance, lighteffect.a*textureColor.a);\n"
}[texture_format]+
"   gl_FragColor = textureColor;\n";
} else if (type === "text") {
result = result +   "    if (textureColor.a < 0.1)\n"+
"     discard;\n"+
"   else\n"+
"     gl_FragColor = textureColor;\n";
} else
result = result +   "   gl_FragColor = lighteffect;\n";
//if (fat_lines)
//  result = result +   "   gl_FragColor = vec4(0.0, abs(point.x), abs(point.y), 1.0);"
result = result + "  }\n";
// console.log(result);
return result;
};
/**
* Call gl functions to create and compile shader
* @returns {Object}
* @param { number } shaderType - gl code for shader type
* @param { string } code - code for the shader
*/
rglwidgetClass.prototype.getShader = function(shaderType, code) {
var gl = this.gl, shader;
shader = gl.createShader(shaderType);
gl.shaderSource(shader, code);
gl.compileShader(shader);
if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS) && !gl.isContextLost())
alert(gl.getShaderInfoLog(shader));
return shader;
};
/**
* Handle a texture after its image has been loaded
* @param { Object } texture - the gl texture object
* @param { Object } textureCanvas - the canvas holding the image
*/
rglwidgetClass.prototype.handleLoadedTexture = function(texture, textureCanvas) {
var gl = this.gl || this.initGL();
gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
gl.bindTexture(gl.TEXTURE_2D, texture);
gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, textureCanvas);
gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_NEAREST);
gl.generateMipmap(gl.TEXTURE_2D);
gl.bindTexture(gl.TEXTURE_2D, null);
};
/**
* Get maximum dimension of texture in current browser.
* @returns {number}
*/
rglwidgetClass.prototype.getMaxTexSize = function() {
var gl = this.gl || this.initGL();  
return Math.min(4096, gl.getParameter(gl.MAX_TEXTURE_SIZE));
};
/**
* Load an image to a texture
* @param { string } uri - The image location
* @param { Object } texture - the gl texture object
*/
rglwidgetClass.prototype.loadImageToTexture = function(uri, texture) {
var canvas = this.textureCanvas,
ctx = canvas.getContext("2d"),
image = new Image(),
self = this;
image.onload = function() {
var w = image.width,
h = image.height,
canvasX = self.getPowerOfTwo(w),
canvasY = self.getPowerOfTwo(h),
gl = self.gl || self.initGL(),
maxTexSize = self.getMaxTexSize();
while (canvasX > 1 && canvasY > 1 && (canvasX > maxTexSize || canvasY > maxTexSize)) {
canvasX /= 2;
canvasY /= 2;
}
canvas.width = canvasX;
canvas.height = canvasY;
ctx.imageSmoothingEnabled = true;
ctx.drawImage(image, 0, 0, canvasX, canvasY);
self.handleLoadedTexture(texture, canvas);
self.drawScene();
};
image.src = uri;
};
/**
* Draw text to the texture canvas
* @returns { Object } object with text measurements
* @param { string } text - the text
* @param { number } cex - expansion
* @param { string } family - font family
* @param { number } font - font number
*/
rglwidgetClass.prototype.drawTextToCanvas = function(text, cex, family, font) {
var canvasX, canvasY,
textY,
scaling = 20,
textColour = "white",
backgroundColour = "rgba(0,0,0,0)",
canvas = this.textureCanvas,
ctx = canvas.getContext("2d"),
i, textHeight = 0, textHeights = [], width, widths = [], 
offsetx, offsety = 0, line, lines = [], offsetsx = [],
offsetsy = [], lineoffsetsy = [], fontStrings = [],
maxTexSize = this.getMaxTexSize(),
getFontString = function(i) {
textHeights[i] = scaling*cex[i];
var fontString = textHeights[i] + "px",
family0 = family[i],
font0 = font[i];
if (family0 === "sans")
family0 = "sans-serif";
else if (family0 === "mono")
family0 = "monospace";
fontString = fontString + " " + family0;
if (font0 === 2 || font0 === 4)
fontString = "bold " + fontString;
if (font0 === 3 || font0 === 4)
fontString = "italic " + fontString;
return fontString;
};
cex = this.repeatToLen(cex, text.length);
family = this.repeatToLen(family, text.length);
font = this.repeatToLen(font, text.length);
canvasX = 1;
line = -1;
offsetx = maxTexSize;
for (i = 0; i < text.length; i++)  {
ctx.font = fontStrings[i] = getFontString(i);
width = widths[i] = ctx.measureText(text[i]).width;
if (offsetx + width > maxTexSize) {
line += 1;
offsety = lineoffsetsy[line] = offsety + 2*textHeight;
if (offsety > maxTexSize)
console.error("Too many strings for texture.");
textHeight = 0;
offsetx = 0;
}
textHeight = Math.max(textHeight, textHeights[i]);
offsetsx[i] = offsetx;
offsetx += width;
canvasX = Math.max(canvasX, offsetx);
lines[i] = line;
}
offsety = lineoffsetsy[line] = offsety + 2*textHeight;
for (i = 0; i < text.length; i++) {
offsetsy[i] = lineoffsetsy[lines[i]];
}
canvasX = this.getPowerOfTwo(canvasX);
canvasY = this.getPowerOfTwo(offsety);
canvas.width = canvasX;
canvas.height = canvasY;
ctx.fillStyle = backgroundColour;
ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
ctx.textBaseline = "alphabetic";
for(i = 0; i < text.length; i++) {
ctx.font = fontStrings[i];
ctx.fillStyle = textColour;
ctx.textAlign = "left";
ctx.fillText(text[i], offsetsx[i],  offsetsy[i]);
}
return {canvasX:canvasX, canvasY:canvasY,
widths:widths, textHeights:textHeights,
offsetsx:offsetsx, offsetsy:offsetsy};
};
/**
* Set the gl viewport and scissor test
* @param { number } id - id of subscene
*/
rglwidgetClass.prototype.setViewport = function(id) {
var gl = this.gl || this.initGL(),
vp = this.getObj(id).par3d.viewport,
x = vp.x*this.canvas.width,
y = vp.y*this.canvas.height,
width = vp.width*this.canvas.width,
height = vp.height*this.canvas.height;
this.vp = {x:x, y:y, width:width, height:height};
gl.viewport(x, y, width, height);
gl.scissor(x, y, width, height);
gl.enable(gl.SCISSOR_TEST);
};
/**
* Set the projection matrix for a subscene
* @param { number } id - id of subscene
*/
rglwidgetClass.prototype.setprMatrix = function(id) {
var subscene = this.getObj(id),
embedding = subscene.embeddings.projection;
if (embedding === "replace")
this.prMatrix.makeIdentity();
else
this.setprMatrix(subscene.parent);
if (embedding === "inherit")
return;
// This is based on the Frustum::enclose code from geom.cpp
var bbox = subscene.par3d.bbox,
scale = subscene.par3d.scale,
ranges = [(bbox[1]-bbox[0])*scale[0]/2,
(bbox[3]-bbox[2])*scale[1]/2,
(bbox[5]-bbox[4])*scale[2]/2],
radius = Math.sqrt(this.sumsq(ranges))*1.1; // A bit bigger to handle labels
if (radius <= 0) radius = 1;
var observer = subscene.par3d.observer,
distance = observer[2],
FOV = subscene.par3d.FOV, ortho = FOV === 0,
t = ortho ? 1 : Math.tan(FOV*Math.PI/360),
near = distance - radius,
far = distance + radius,
hlen,
aspect = this.vp.width/this.vp.height,
z = subscene.par3d.zoom;
if (far < 0.0)
far = 1.0;
if (near < far/100.0)
near = far/100.0;
hlen = t*near;
if (ortho) {
if (aspect > 1)
this.prMatrix.ortho(-hlen*aspect*z, hlen*aspect*z,
-hlen*z, hlen*z, near, far);
else
this.prMatrix.ortho(-hlen*z, hlen*z,
-hlen*z/aspect, hlen*z/aspect,
near, far);
} else {
if (aspect > 1)
this.prMatrix.frustum(-hlen*aspect*z, hlen*aspect*z,
-hlen*z, hlen*z, near, far);
else
this.prMatrix.frustum(-hlen*z, hlen*z,
-hlen*z/aspect, hlen*z/aspect,
near, far);
}
};
/**
* Set the model-view matrix for a subscene
* @param { number } id - id of the subscene
*/
rglwidgetClass.prototype.setmvMatrix = function(id) {
var observer = this.getObj(id).par3d.observer;
this.mvMatrix.makeIdentity();
this.setmodelMatrix(id);
this.mvMatrix.translate(-observer[0], -observer[1], -observer[2]);
};
/**
* Set the model matrix for a subscene
* @param { number } id - id of the subscene
*/
rglwidgetClass.prototype.setmodelMatrix = function(id) {
var subscene = this.getObj(id),
embedding = subscene.embeddings.model;
if (embedding !== "inherit") {
var scale = subscene.par3d.scale,
bbox = subscene.par3d.bbox,
center = [(bbox[0]+bbox[1])/2,
(bbox[2]+bbox[3])/2,
(bbox[4]+bbox[5])/2];
this.mvMatrix.translate(-center[0], -center[1], -center[2]);
this.mvMatrix.scale(scale[0], scale[1], scale[2]);
this.mvMatrix.multRight( subscene.par3d.userMatrix );
}
if (embedding !== "replace")
this.setmodelMatrix(subscene.parent);
};
/**
* Set the normals matrix for a subscene
* @param { number } subsceneid - id of the subscene
*/
rglwidgetClass.prototype.setnormMatrix = function(subsceneid) {
var self = this,
recurse = function(id) {
var sub = self.getObj(id),
embedding = sub.embeddings.model;
if (embedding !== "inherit") {
var scale = sub.par3d.scale;
self.normMatrix.scale(1/scale[0], 1/scale[1], 1/scale[2]);
self.normMatrix.multRight(sub.par3d.userMatrix);
}
if (embedding !== "replace")
recurse(sub.parent);
};
self.normMatrix.makeIdentity();
recurse(subsceneid);
};
/**
* Set the combined projection-model-view matrix
*/
rglwidgetClass.prototype.setprmvMatrix = function() {
this.prmvMatrix = new CanvasMatrix4( this.mvMatrix );
this.prmvMatrix.multRight( this.prMatrix );
};
/**
* Count clipping planes in a scene
* @returns {number}
*/
rglwidgetClass.prototype.countClipplanes = function() {
return this.countObjs("clipplanes");
};
/**
* Count lights in a scene
* @returns { number }
*/
rglwidgetClass.prototype.countLights = function() {
return this.countObjs("light");
};
/**
* Count objects of specific type in a scene
* @returns { number }
* @param { string } type - Type of object to count
*/
rglwidgetClass.prototype.countObjs = function(type) {
var self = this,
bound = 0;
Object.keys(this.scene.objects).forEach(
function(key) {
if (self.getObj(parseInt(key, 10)).type === type)
bound = bound + 1;
});
return bound;
};
/**
* Initialize a subscene
* @param { number } id - id of subscene.
*/
rglwidgetClass.prototype.initSubscene = function(id) {
var sub = this.getObj(id),
i, obj;
if (sub.type !== "subscene")
return;
sub.par3d.userMatrix = this.toCanvasMatrix4(sub.par3d.userMatrix);
sub.par3d.listeners = [].concat(sub.par3d.listeners);
sub.backgroundId = undefined;
sub.subscenes = [];
sub.clipplanes = [];
sub.transparent = [];
sub.opaque = [];
sub.lights = [];
for (i=0; i < sub.objects.length; i++) {
obj = this.getObj(sub.objects[i]);
if (typeof obj === "undefined") {
sub.objects.splice(i, 1);
i--;
} else if (obj.type === "background")
sub.backgroundId = obj.id;
else
sub[this.whichList(obj.id)].push(obj.id);
}
};
/**
* Copy object
* @param { number } id - id of object to copy
* @param { string } reuse - Document id of scene to reuse
*/
rglwidgetClass.prototype.copyObj = function(id, reuse) {
var obj = this.getObj(id),
prev = document.getElementById(reuse);
if (prev !== null) {
prev = prev.rglinstance;
var
prevobj = prev.getObj(id),
fields = ["flags", "type",
"colors", "vertices", "centers",
"normals", "offsets",
"texts", "cex", "family", "font", "adj",
"material",
"radii",
"texcoords",
"userMatrix", "ids",
"dim",
"par3d", "userMatrix",
"viewpoint", "finite"],
i;
for (i = 0; i < fields.length; i++) {
if (typeof prevobj[fields[i]] !== "undefined")
obj[fields[i]] = prevobj[fields[i]];
}
} else
console.warn("copyObj failed");
};
/**
* Update the triangles used to display a plane
* @param { number } id - id of the plane
* @param { Object } bbox - bounding box in which to display the plane
*/
rglwidgetClass.prototype.planeUpdateTriangles = function(id, bbox) {
var perms = [[0,0,1], [1,2,2], [2,1,0]],
x, xrow, elem, A, d, nhits, i, j, k, u, v, w, intersect, which, v0, v2, vx, reverse,
face1 = [], face2 = [], normals = [],
obj = this.getObj(id),
nPlanes = obj.normals.length;
obj.bbox = bbox;
obj.vertices = [];
obj.initialized = false;
for (elem = 0; elem < nPlanes; elem++) {
//    Vertex Av = normal.getRecycled(elem);
x = [];
A = obj.normals[elem];
d = obj.offsets[elem][0];
nhits = 0;
for (i=0; i<3; i++)
for (j=0; j<2; j++)
for (k=0; k<2; k++) {
u = perms[0][i];
v = perms[1][i];
w = perms[2][i];
if (A[w] !== 0.0) {
intersect = -(d + A[u]*bbox[j+2*u] + A[v]*bbox[k+2*v])/A[w];
if (bbox[2*w] < intersect && intersect < bbox[1+2*w]) {
xrow = [];
xrow[u] = bbox[j+2*u];
xrow[v] = bbox[k+2*v];
xrow[w] = intersect;
x.push(xrow);
face1[nhits] = j + 2*u;
face2[nhits] = k + 2*v;
nhits++;
}
}
}
if (nhits > 3) {
/* Re-order the intersections so the triangles work */
for (i=0; i<nhits-2; i++) {
which = 0; /* initialize to suppress warning */
for (j=i+1; j<nhits; j++) {
if (face1[i] == face1[j] || face1[i] == face2[j] ||
face2[i] == face1[j] || face2[i] == face2[j] ) {
which = j;
break;
}
}
if (which > i+1) {
this.swap(x, i+1, which);
this.swap(face1, i+1, which);
this.swap(face2, i+1, which);
}
}
}
if (nhits >= 3) {
/* Put in order so that the normal points out the FRONT of the faces */
v0 = [x[0][0] - x[1][0] , x[0][1] - x[1][1], x[0][2] - x[1][2]];
v2 = [x[2][0] - x[1][0] , x[2][1] - x[1][1], x[2][2] - x[1][2]];
/* cross-product */
vx = this.xprod(v0, v2);
reverse = this.dotprod(vx, A) > 0;
for (i=0; i<nhits-2; i++) {
obj.vertices.push(x[0]);
normals.push(A);
for (j=1; j<3; j++) {
obj.vertices.push(x[i + (reverse ? 3-j : j)]);
normals.push(A);
}
}
}
}
obj.pnormals = normals;
};
/**
* Initialize object for display
* @param { number } id - id of object to initialize
*/
rglwidgetClass.prototype.initObj = function(id) {
var obj = this.getObj(id),
flags = obj.flags,
type = obj.type,
is_lit = flags & this.f_is_lit,
is_lines = flags & this.f_is_lines,
fat_lines = flags & this.f_fat_lines,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
is_transparent = obj.is_transparent,
depth_sort = flags & this.f_depth_sort,
sprites_3d = flags & this.f_sprites_3d,
sprite_3d = flags & this.f_sprite_3d,
fixed_size = flags & this.f_fixed_size,
is_twosided = (flags & this.f_is_twosided) > 0,
is_brush = flags & this.f_is_brush,
gl = this.gl || this.initGL(),
texinfo, drawtype, nclipplanes, f, nrows, oldrows,
i,j,v,v1,v2, mat, uri, matobj, pass, passes, pmode,
dim, nx, nz, attr;
if (typeof id !== "number") {
this.alertOnce("initObj id is "+typeof id);
}
obj.initialized = true;
if (type === "bboxdeco" || type === "subscene")
return;
if (type === "light") {
obj.ambient = new Float32Array(obj.colors[0].slice(0,3));
obj.diffuse = new Float32Array(obj.colors[1].slice(0,3));
obj.specular = new Float32Array(obj.colors[2].slice(0,3));
obj.lightDir = new Float32Array(obj.vertices[0]);
return;
}
if (type === "clipplanes") {
obj.vClipplane = this.flatten(this.cbind(obj.normals, obj.offsets));
return;
}
if (type === "background" && typeof obj.ids !== "undefined") {
obj.quad = this.flatten([].concat(obj.ids));
return;
}
if (is_transparent) {
depth_sort = ["triangles", "quads", "surface",
"spheres", "sprites", "text"].indexOf(type) >= 0;
}
if (is_brush)
this.initSelection(id);
if (typeof obj.vertices === "undefined")
obj.vertices = [];
v = obj.vertices;
obj.vertexCount = v.length;
if (!obj.vertexCount) return;
if (is_twosided) {
if (typeof obj.userAttributes === "undefined")
obj.userAttributes = {};
v1 = Array(v.length);
v2 = Array(v.length);
if (obj.type == "triangles" || obj.type == "quads") {
if (obj.type == "triangles")
nrow = 3;
else
nrow = 4;
for (i=0; i<Math.floor(v.length/nrow); i++)
for (j=0; j<nrow; j++) {
v1[nrow*i + j] = v[nrow*i + ((j+1) % nrow)];
v2[nrow*i + j] = v[nrow*i + ((j+2) % nrow)];
}
} else if (obj.type == "surface") {
dim = obj.dim[0];
nx = dim[0];
nz = dim[1];
for (j=0; j<nx; j++) {
for (i=0; i<nz; i++) {
if (i+1 < nz && j+1 < nx) {
v2[j + nx*i] = v[j + nx*(i+1)];
v1[j + nx*i] = v[j+1 + nx*(i+1)];
} else if (i+1 < nz) {
v2[j + nx*i] = v[j-1 + nx*i];
v1[j + nx*i] = v[j + nx*(i+1)];
} else {
v2[j + nx*i] = v[j + nx*(i-1)];
v1[j + nx*i] = v[j-1 + nx*(i-1)];
}
}
}
}
obj.userAttributes.aPos1 = v1;
obj.userAttributes.aPos2 = v2;
}
if (!sprites_3d) {
if (gl.isContextLost()) return;
obj.prog = gl.createProgram();
gl.attachShader(obj.prog, this.getShader( gl.VERTEX_SHADER,
this.getVertexShader(id) ));
gl.attachShader(obj.prog, this.getShader( gl.FRAGMENT_SHADER,
this.getFragmentShader(id) ));
//  Force aPos to location 0, aCol to location 1
gl.bindAttribLocation(obj.prog, 0, "aPos");
gl.bindAttribLocation(obj.prog, 1, "aCol");
gl.linkProgram(obj.prog);
var linked = gl.getProgramParameter(obj.prog, gl.LINK_STATUS);
if (!linked) {
// An error occurred while linking
var lastError = gl.getProgramInfoLog(obj.prog);
console.warn("Error in program linking:" + lastError);
gl.deleteProgram(obj.prog);
return;
}
}
if (type === "text") {
texinfo = this.drawTextToCanvas(obj.texts,
this.flatten(obj.cex),
this.flatten(obj.family),
this.flatten(obj.family));
}
if (fixed_quads && !sprites_3d) {
obj.ofsLoc = gl.getAttribLocation(obj.prog, "aOfs");
}
if (sprite_3d) {
obj.origLoc = gl.getUniformLocation(obj.prog, "uOrig");
obj.sizeLoc = gl.getUniformLocation(obj.prog, "uSize");
obj.usermatLoc = gl.getUniformLocation(obj.prog, "usermat");
}
if (has_texture || type == "text") {
if (!obj.texture)
obj.texture = gl.createTexture();
obj.texLoc = gl.getAttribLocation(obj.prog, "aTexcoord");
obj.sampler = gl.getUniformLocation(obj.prog, "uSampler");
}
if (has_texture) {
mat = obj.material;
if (typeof mat.uri !== "undefined")
uri = mat.uri;
else if (typeof mat.uriElementId === "undefined") {
matobj = this.getObj(mat.uriId);
if (typeof matobj !== "undefined") {
uri = matobj.material.uri;
} else {
uri = "";
}
} else
uri = document.getElementById(mat.uriElementId).rglinstance.getObj(mat.uriId).material.uri;
this.loadImageToTexture(uri, obj.texture);
}
if (type === "text") {
this.handleLoadedTexture(obj.texture, this.textureCanvas);
}
var stride = 3, nc, cofs, nofs, radofs, oofs, tofs, vnew, fnew,
nextofs = -1, pointofs = -1, alias, colors, key, selection, filter;
obj.alias = undefined;
colors = obj.colors;
j = this.scene.crosstalk.id.indexOf(id);
if (j >= 0) {
key = this.scene.crosstalk.key[j];
options = this.scene.crosstalk.options[j];
colors = colors.slice(0); 
for (i = 0; i < v.length; i++)
colors[i] = obj.colors[i % obj.colors.length].slice(0);
if ( (selection = this.scene.crosstalk.selection) &&
(selection.length || !options.selectedIgnoreNone) )
for (i = 0; i < v.length; i++) {
if (!selection.includes(key[i])) {
if (options.deselectedColor)
colors[i] = options.deselectedColor.slice(0);
colors[i][3] = colors[i][3]*options.deselectedFade;   /* default: mostly transparent if not selected */
} else if (options.selectedColor)
colors[i] = options.selectedColor.slice(0);
}
if ( (filter = this.scene.crosstalk.filter) )
for (i = 0; i < v.length; i++) 
if (!filter.includes(key[i])) {
if (options.filteredColor)
colors[i] = options.filteredColor.slice(0);
colors[i][3] = colors[i][3]*options.filteredFade;   /* default: completely hidden if filtered */
}
}  
nc = obj.colorCount = colors.length;
if (nc > 1) {
cofs = stride;
stride = stride + 4;
v = this.cbind(v, colors);
} else {
cofs = -1;
obj.onecolor = this.flatten(colors);
}
if (typeof obj.normals !== "undefined") {
nofs = stride;
stride = stride + 3;
v = this.cbind(v, typeof obj.pnormals !== "undefined" ? obj.pnormals : obj.normals);
} else
nofs = -1;
if (typeof obj.radii !== "undefined") {
radofs = stride;
stride = stride + 1;
// FIXME:  always concat the radii?
if (obj.radii.length === v.length) {
v = this.cbind(v, obj.radii);
} else if (obj.radii.length === 1) {
v = v.map(function(row, i, arr) { return row.concat(obj.radii[0]);});
}
} else
radofs = -1;
// Add default indices
f = Array(v.length);
for (i = 0; i < v.length; i++)
f[i] = i;
obj.f = [f,f];
if (type == "sprites" && !sprites_3d) {
tofs = stride;
stride += 2;
oofs = stride;
stride += 2;
vnew = new Array(4*v.length);
fnew = new Array(4*v.length);
alias = new Array(v.length);
var rescale = fixed_size ? 72 : 1,
size = obj.radii, s = rescale*size[0]/2;
last = v.length;
f = obj.f[0];
for (i=0; i < v.length; i++) {
if (size.length > 1)
s = rescale*size[i]/2;
vnew[i]  = v[i].concat([0,0,-s,-s]);
fnew[4*i] = f[i];
vnew[last]= v[i].concat([1,0, s,-s]);
fnew[4*i+1] = last++;
vnew[last]= v[i].concat([1,1, s, s]);
fnew[4*i+2] = last++;
vnew[last]= v[i].concat([0,1,-s, s]);
fnew[4*i+3] = last++;
alias[i] = [last-3, last-2, last-1];
}
v = vnew;
obj.vertexCount = v.length;
obj.f = [fnew, fnew];
} else if (type === "text") {
tofs = stride;
stride += 2;
oofs = stride;
stride += 2;
vnew = new Array(4*v.length);
f = obj.f[0];
fnew = new Array(4*f.length);
alias = new Array(v.length);
last = v.length;
for (i=0; i < v.length; i++) {
vnew[i]  = v[i].concat([0,-0.5]).concat(obj.adj[0]);
fnew[4*i] = f[i];
vnew[last] = v[i].concat([1,-0.5]).concat(obj.adj[0]);
fnew[4*i+1] = last++;
vnew[last] = v[i].concat([1, 1.5]).concat(obj.adj[0]);
fnew[4*i+2] = last++;
vnew[last] = v[i].concat([0, 1.5]).concat(obj.adj[0]);
fnew[4*i+3] = last++;
alias[i] = [last-3, last-2, last-1];
for (j=0; j < 4; j++) {
v1 = vnew[fnew[4*i+j]];
v1[tofs+2] = 2*(v1[tofs]-v1[tofs+2])*texinfo.widths[i];
v1[tofs+3] = 2*(v1[tofs+1]-v1[tofs+3])*texinfo.textHeights[i];
v1[tofs] = (texinfo.offsetsx[i] + v1[tofs]*texinfo.widths[i])/texinfo.canvasX;
v1[tofs+1] = 1.0-(texinfo.offsetsy[i] -
v1[tofs+1]*texinfo.textHeights[i])/texinfo.canvasY;
vnew[fnew[4*i+j]] = v1;
}
}
v = vnew;
obj.vertexCount = v.length;
obj.f = [fnew, fnew];
} else if (typeof obj.texcoords !== "undefined") {
tofs = stride;
stride += 2;
oofs = -1;
v = this.cbind(v, obj.texcoords);
} else {
tofs = -1;
oofs = -1;
}
obj.alias = alias;
if (typeof obj.userAttributes !== "undefined") {
obj.userAttribOffsets = {};
obj.userAttribLocations = {};
obj.userAttribSizes = {};
for (attr in obj.userAttributes) {
obj.userAttribLocations[attr] = gl.getAttribLocation(obj.prog, attr);
if (obj.userAttribLocations[attr] >= 0) { // Attribute may not have been used
obj.userAttribOffsets[attr] = stride;
v = this.cbind(v, obj.userAttributes[attr]);
stride = v[0].length;
obj.userAttribSizes[attr] = stride - obj.userAttribOffsets[attr];
}
}
}
if (typeof obj.userUniforms !== "undefined") {
obj.userUniformLocations = {};
for (attr in obj.userUniforms)
obj.userUniformLocations[attr] = gl.getUniformLocation(obj.prog, attr);
}
if (sprites_3d) {
obj.userMatrix = new CanvasMatrix4(obj.userMatrix);
obj.objects = this.flatten([].concat(obj.ids));
is_lit = false;
}
if (is_lit && !fixed_quads) {
obj.normLoc = gl.getAttribLocation(obj.prog, "aNorm");
}
nclipplanes = this.countClipplanes();
if (nclipplanes && !sprites_3d) {
obj.clipLoc = [];
for (i=0; i < nclipplanes; i++)
obj.clipLoc[i] = gl.getUniformLocation(obj.prog,"vClipplane" + i);
}
if (is_lit) {
obj.emissionLoc = gl.getUniformLocation(obj.prog, "emission");
obj.emission = new Float32Array(this.stringToRgb(this.getMaterial(id, "emission")));
obj.shininessLoc = gl.getUniformLocation(obj.prog, "shininess");
obj.shininess = this.getMaterial(id, "shininess");
obj.nlights = this.countLights();
obj.ambientLoc = [];
obj.ambient = new Float32Array(this.stringToRgb(this.getMaterial(id, "ambient")));
obj.specularLoc = [];
obj.specular = new Float32Array(this.stringToRgb(this.getMaterial(id, "specular")));
obj.diffuseLoc = [];
obj.lightDirLoc = [];
obj.viewpointLoc = [];
obj.finiteLoc = [];
for (i=0; i < obj.nlights; i++) {
obj.ambientLoc[i] = gl.getUniformLocation(obj.prog, "ambient" + i);
obj.specularLoc[i] = gl.getUniformLocation(obj.prog, "specular" + i);
obj.diffuseLoc[i] = gl.getUniformLocation(obj.prog, "diffuse" + i);
obj.lightDirLoc[i] = gl.getUniformLocation(obj.prog, "lightDir" + i);
obj.viewpointLoc[i] = gl.getUniformLocation(obj.prog, "viewpoint" + i);
obj.finiteLoc[i] = gl.getUniformLocation(obj.prog, "finite" + i);
}
}
obj.passes = is_twosided + 1;
obj.pmode = new Array(obj.passes);
for (pass = 0; pass < obj.passes; pass++) {
if (type === "triangles" || type === "quads" || type === "surface")
pmode = this.getMaterial(id, (pass === 0) ? "front" : "back");
else pmode = "filled";
obj.pmode[pass] = pmode;
}
obj.f.length = obj.passes;
for (pass = 0; pass < obj.passes; pass++) {
f = fnew = obj.f[pass];
pmode = obj.pmode[pass];
if (pmode === "culled")
f = [];
else if (pmode === "points") {
// stay with default
} else if ((type === "quads" || type === "text" ||
type === "sprites") && !sprites_3d) {
nrows = Math.floor(obj.vertexCount/4);
if (pmode === "filled") {
fnew = Array(6*nrows);
for (i=0; i < nrows; i++) {
fnew[6*i] = f[4*i];
fnew[6*i+1] = f[4*i + 1];
fnew[6*i+2] = f[4*i + 2];
fnew[6*i+3] = f[4*i];
fnew[6*i+4] = f[4*i + 2];
fnew[6*i+5] = f[4*i + 3];
}
} else {
fnew = Array(8*nrows);
for (i=0; i < nrows; i++) {
fnew[8*i] = f[4*i];
fnew[8*i+1] = f[4*i + 1];
fnew[8*i+2] = f[4*i + 1];
fnew[8*i+3] = f[4*i + 2];
fnew[8*i+4] = f[4*i + 2];
fnew[8*i+5] = f[4*i + 3];
fnew[8*i+6] = f[4*i + 3];
fnew[8*i+7] = f[4*i];
}
}
} else if (type === "triangles") {
nrows = Math.floor(obj.vertexCount/3);
if (pmode === "filled") {
fnew = Array(3*nrows);
for (i=0; i < fnew.length; i++) {
fnew[i] = f[i];
}
} else if (pmode === "lines") {
fnew = Array(6*nrows);
for (i=0; i < nrows; i++) {
fnew[6*i] = f[3*i];
fnew[6*i + 1] = f[3*i + 1];
fnew[6*i + 2] = f[3*i + 1];
fnew[6*i + 3] = f[3*i + 2];
fnew[6*i + 4] = f[3*i + 2];
fnew[6*i + 5] = f[3*i];
}
}
} else if (type === "spheres") {
// default
} else if (type === "surface") {
dim = obj.dim[0];
nx = dim[0];
nz = dim[1];
if (pmode === "filled") {
fnew = [];
for (j=0; j<nx-1; j++) {
for (i=0; i<nz-1; i++) {
fnew.push(f[j + nx*i],
f[j + nx*(i+1)],
f[j + 1 + nx*(i+1)],
f[j + nx*i],
f[j + 1 + nx*(i+1)],
f[j + 1 + nx*i]);
}
}
} else if (pmode === "lines") {
fnew = [];
for (j=0; j<nx; j++) {
for (i=0; i<nz; i++) {
if (i+1 < nz)
fnew.push(f[j + nx*i],
f[j + nx*(i+1)]);
if (j+1 < nx)
fnew.push(f[j + nx*i],
f[j+1 + nx*i]);
}
}
}
}
obj.f[pass] = fnew;
if (depth_sort) {
drawtype = "DYNAMIC_DRAW";
} else {
drawtype = "STATIC_DRAW";
}
}
if (fat_lines) {
alias = undefined;
obj.nextLoc = gl.getAttribLocation(obj.prog, "aNext");
obj.pointLoc = gl.getAttribLocation(obj.prog, "aPoint");
obj.aspectLoc = gl.getUniformLocation(obj.prog, "uAspect");
obj.lwdLoc = gl.getUniformLocation(obj.prog, "uLwd");
// Expand vertices to turn each segment into a pair of triangles
for (pass = 0; pass < obj.passes; pass++) {
f = obj.f[pass];    
oldrows = f.length;
if (obj.pmode[pass] === "lines") 
break;
}
if (type === "linestrip") 
nrows = 4*(oldrows - 1); 
else
nrows = 2*oldrows;
vnew = new Array(nrows);
fnew = new Array(1.5*nrows);
var fnext = new Array(nrows),
fpt = new Array(nrows), 
pt, start, gap = type === "linestrip" ? 3 : 1;
// We're going to turn each pair of vertices into 4 new ones, with the "next" and "pt" attributes
// added.
// We do this by copying the originals in the first pass, adding the new attributes, then in a 
// second pass add new vertices at the end.
for (i = 0; i < v.length; i++) {
vnew[i] = v[i].concat([0,0,0,0,0]); 
}
nextofs = stride;
pointofs = stride + 3;
stride = stride + 5;
// Now add the extras
last = v.length - 1;
ind = 0;
alias = new Array(f.length);
for (i = 0; i < f.length; i++)
alias[i] = [];
for (i = 0; i < f.length - 1; i++) {
if (type !== "linestrip" && i % 2 == 1)
continue;
k = ++last;
vnew[k] = vnew[f[i]].slice();
for (j=0; j<3; j++)
vnew[k][nextofs + j] = vnew[f[i+1]][j];
vnew[k][pointofs] = -1;
vnew[k][pointofs+1] = -1;
fnew[ind] = k;
last++;
vnew[last] = vnew[k].slice();
vnew[last][pointofs] = 1;
fnew[ind+1] = last;
alias[f[i]].push(last-1, last);
last++;
k = last;
vnew[k] = vnew[f[i+1]].slice();
for (j=0; j<3; j++)
vnew[k][nextofs + j] = vnew[f[i]][j];
vnew[k][pointofs] = -1;
vnew[k][pointofs+1] = 1;
fnew[ind+2] = k;
fnew[ind+3] = fnew[ind+1];
last++;
vnew[last] = vnew[k].slice();
vnew[last][pointofs] = 1;
fnew[ind+4] = last;
fnew[ind+5] = fnew[ind+2];
ind += 6;
alias[f[i+1]].push(last-1, last);
}
vnew.length = last+1;
v = vnew;
obj.vertexCount = v.length;
if (typeof alias !== "undefined" && typeof obj.alias !== "undefined") {  // Already have aliases from previous section?
var oldalias = obj.alias, newalias = Array(obj.alias.length);
for (i = 0; i < newalias.length; i++) {
newalias[i] = oldalias[i].slice();
for (j = 0; j < oldalias[i].length; j++)
Array.prototype.push.apply(newalias[i], alias[oldalias[j]]); // pushes each element 
}
obj.alias = newalias;
} else
obj.alias = alias;
for (pass = 0; pass < obj.passes; pass++)
if (type === "lines" || type === "linestrip" || obj.pmode[pass] == "lines") {
obj.f[pass] = fnew;
}
if (depth_sort) 
drawtype = "DYNAMIC_DRAW";
else
drawtype = "STATIC_DRAW";
}
for (pass = 0; pass < obj.passes; pass++) {
if (obj.vertexCount > 65535) {
if (this.index_uint) {
obj.f[pass] = new Uint32Array(obj.f[pass]);
obj.index_uint = true;
} else
this.alertOnce("Object has "+obj.vertexCount+" vertices, not supported in this browser.");
} else {
obj.f[pass] = new Uint16Array(obj.f[pass]);
obj.index_uint = false;
}
}
if (stride !== v[0].length) {
this.alertOnce("problem in stride calculation");
}
obj.vOffsets = {vofs:0, cofs:cofs, nofs:nofs, radofs:radofs, oofs:oofs, tofs:tofs,
nextofs:nextofs, pointofs:pointofs, stride:stride};
obj.values = new Float32Array(this.flatten(v));
if (type !== "spheres" && !sprites_3d) {
obj.buf = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW); //
obj.ibuf = Array(obj.passes);
obj.ibuf[0] = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[0]);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[0], gl[drawtype]);
if (is_twosided) {
obj.ibuf[1] = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[1]);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[1], gl[drawtype]);
}
}
if (!sprites_3d) {
obj.mvMatLoc = gl.getUniformLocation(obj.prog, "mvMatrix");
obj.prMatLoc = gl.getUniformLocation(obj.prog, "prMatrix");
}
if (fixed_size) {
obj.textScaleLoc = gl.getUniformLocation(obj.prog, "textScale");
}
if (is_lit && !sprites_3d) {
obj.normMatLoc = gl.getUniformLocation(obj.prog, "normMatrix");
}
if (is_twosided) {
obj.frontLoc = gl.getUniformLocation(obj.prog, "front");
}
};
/**
* Set gl depth test based on object's material
* @param { number } id - object to use
*/
rglwidgetClass.prototype.setDepthTest = function(id) {
var gl = this.gl || this.initGL(),
tests = {never: gl.NEVER,
less:  gl.LESS,
equal: gl.EQUAL,
lequal:gl.LEQUAL,
greater: gl.GREATER,
notequal: gl.NOTEQUAL,
gequal: gl.GEQUAL,
always: gl.ALWAYS},
test = tests[this.getMaterial(id, "depth_test")];
gl.depthFunc(test);
};
rglwidgetClass.prototype.mode4type = {points : "POINTS",
linestrip : "LINE_STRIP",
abclines : "LINES",
lines : "LINES",
sprites : "TRIANGLES",
planes : "TRIANGLES",
text : "TRIANGLES",
quads : "TRIANGLES",
surface : "TRIANGLES",
triangles : "TRIANGLES"};
/**
* Sort objects from back to front
* @returns { number[] }
* @param { Object } obj - object to sort
*/
rglwidgetClass.prototype.depthSort = function(obj) {
var n = obj.centers.length,
depths = new Float32Array(n),
result = new Array(n),
compare = function(i,j) { return depths[j] - depths[i]; },
z, w;
for(i=0; i<n; i++) {
z = this.prmvMatrix.m13*obj.centers[i][0] +
this.prmvMatrix.m23*obj.centers[i][1] +
this.prmvMatrix.m33*obj.centers[i][2] +
this.prmvMatrix.m43;
w = this.prmvMatrix.m14*obj.centers[i][0] +
this.prmvMatrix.m24*obj.centers[i][1] +
this.prmvMatrix.m34*obj.centers[i][2] +
this.prmvMatrix.m44;
depths[i] = z/w;
result[i] = i;
}
result.sort(compare);
return result;
};
/**
* Draw an object in a subscene
* @param { number } id - object to draw
* @param { number } subsceneid - id of subscene
*/
rglwidgetClass.prototype.drawObj = function(id, subsceneid) {
var obj = this.getObj(id),
subscene = this.getObj(subsceneid),
flags = obj.flags,
type = obj.type,
is_lit = flags & this.f_is_lit,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
is_transparent = flags & this.f_is_transparent,
depth_sort = flags & this.f_depth_sort,
sprites_3d = flags & this.f_sprites_3d,
sprite_3d = flags & this.f_sprite_3d,
is_lines = flags & this.f_is_lines,
fat_lines = flags & this.f_fat_lines,
is_points = flags & this.f_is_points,
fixed_size = flags & this.f_fixed_size,
is_twosided = (flags & this.f_is_twosided) > 0,
gl = this.gl || this.initGL(),
mat,
sphereMV, baseofs, ofs, sscale, i, count, light,
pass, mode, pmode, attr;
if (typeof id !== "number") {
this.alertOnce("drawObj id is "+typeof id);
}
if (type === "planes") {
if (obj.bbox !== subscene.par3d.bbox || !obj.initialized) {
this.planeUpdateTriangles(id, subscene.par3d.bbox);
}
}
if (!obj.initialized)
this.initObj(id);
if (type === "clipplanes") {
count = obj.offsets.length;
var IMVClip = [];
for (i=0; i < count; i++) {
IMVClip[i] = this.multMV(this.invMatrix, obj.vClipplane.slice(4*i, 4*(i+1)));
}
obj.IMVClip = IMVClip;
return;
}
if (type === "light" || type === "bboxdeco" || !obj.vertexCount)
return;
if (!is_transparent &&
obj.someHidden) {
is_transparent = true;
depth_sort = ["triangles", "quads", "surface",
"spheres", "sprites", "text"].indexOf(type) >= 0;
}        
this.setDepthTest(id);
if (sprites_3d) {
var norigs = obj.vertices.length,
savenorm = new CanvasMatrix4(this.normMatrix);
this.origs = obj.vertices;
this.usermat = new Float32Array(obj.userMatrix.getAsArray());
this.radii = obj.radii;
this.normMatrix = subscene.spriteNormmat;
for (this.iOrig=0; this.iOrig < norigs; this.iOrig++) {
for (i=0; i < obj.objects.length; i++) {
this.drawObj(obj.objects[i], subsceneid);
}
}
this.normMatrix = savenorm;
return;
} else {
gl.useProgram(obj.prog);
}
if (sprite_3d) {
gl.uniform3fv(obj.origLoc, new Float32Array(this.origs[this.iOrig]));
if (this.radii.length > 1) {
gl.uniform1f(obj.sizeLoc, this.radii[this.iOrig][0]);
} else {
gl.uniform1f(obj.sizeLoc, this.radii[0][0]);
}
gl.uniformMatrix4fv(obj.usermatLoc, false, this.usermat);
}
if (type === "spheres") {
gl.bindBuffer(gl.ARRAY_BUFFER, this.sphere.buf);
} else {
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
}
gl.uniformMatrix4fv( obj.prMatLoc, false, new Float32Array(this.prMatrix.getAsArray()) );
gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(this.mvMatrix.getAsArray()) );
var clipcheck = 0,
clipplaneids = subscene.clipplanes,
clip, j;
for (i=0; i < clipplaneids.length; i++) {
clip = this.getObj(clipplaneids[i]);
for (j=0; j < clip.offsets.length; j++) {
gl.uniform4fv(obj.clipLoc[clipcheck + j], clip.IMVClip[j]);
}
clipcheck += clip.offsets.length;
}
if (typeof obj.clipLoc !== "undefined")
for (i=clipcheck; i < obj.clipLoc.length; i++)
gl.uniform4f(obj.clipLoc[i], 0,0,0,0);
if (is_lit) {
gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(this.normMatrix.getAsArray()) );
gl.uniform3fv( obj.emissionLoc, obj.emission);
gl.uniform1f( obj.shininessLoc, obj.shininess);
for (i=0; i < subscene.lights.length; i++) {
light = this.getObj(subscene.lights[i]);
gl.uniform3fv( obj.ambientLoc[i], this.componentProduct(light.ambient, obj.ambient));
gl.uniform3fv( obj.specularLoc[i], this.componentProduct(light.specular, obj.specular));
gl.uniform3fv( obj.diffuseLoc[i], light.diffuse);
gl.uniform3fv( obj.lightDirLoc[i], light.lightDir);
gl.uniform1i( obj.viewpointLoc[i], light.viewpoint);
gl.uniform1i( obj.finiteLoc[i], light.finite);
}
for (i=subscene.lights.length; i < obj.nlights; i++) {
gl.uniform3f( obj.ambientLoc[i], 0,0,0);
gl.uniform3f( obj.specularLoc[i], 0,0,0);
gl.uniform3f( obj.diffuseLoc[i], 0,0,0);
}
}
if (fixed_size) {
gl.uniform2f( obj.textScaleLoc, 0.75/this.vp.width, 0.75/this.vp.height);
}
gl.enableVertexAttribArray( this.posLoc );
var nc = obj.colorCount;
count = obj.vertexCount;
if (type === "spheres") {
subscene = this.getObj(subsceneid);
var scale = subscene.par3d.scale,
scount = count, indices;
gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,  0);
gl.enableVertexAttribArray(obj.normLoc );
gl.vertexAttribPointer(obj.normLoc,  3, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,  0);
gl.disableVertexAttribArray( this.colLoc );
var sphereNorm = new CanvasMatrix4();
sphereNorm.scale(scale[0], scale[1], scale[2]);
sphereNorm.multRight(this.normMatrix);
gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(sphereNorm.getAsArray()) );
if (nc == 1) {
gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
}
if (has_texture) {
gl.enableVertexAttribArray( obj.texLoc );
gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,
4*this.sphere.vOffsets.tofs);
gl.activeTexture(gl.TEXTURE0);
gl.bindTexture(gl.TEXTURE_2D, obj.texture);
gl.uniform1i( obj.sampler, 0);
}
if (depth_sort)
indices = this.depthSort(obj);
for (i = 0; i < scount; i++) {
sphereMV = new CanvasMatrix4();
if (depth_sort) {
baseofs = indices[i]*obj.vOffsets.stride;
} else {
baseofs = i*obj.vOffsets.stride;
}
ofs = baseofs + obj.vOffsets.radofs;
sscale = obj.values[ofs];
sphereMV.scale(sscale/scale[0], sscale/scale[1], sscale/scale[2]);
sphereMV.translate(obj.values[baseofs],
obj.values[baseofs+1],
obj.values[baseofs+2]);
sphereMV.multRight(this.mvMatrix);
gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(sphereMV.getAsArray()) );
if (nc > 1) {
ofs = baseofs + obj.vOffsets.cofs;
gl.vertexAttrib4f( this.colLoc, obj.values[ofs],
obj.values[ofs+1],
obj.values[ofs+2],
obj.values[ofs+3] );
}
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.sphere.ibuf);
gl.drawElements(gl.TRIANGLES, this.sphere.sphereCount, gl.UNSIGNED_SHORT, 0);
}
return;
} else {
if (obj.colorCount === 1) {
gl.disableVertexAttribArray( this.colLoc );
gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
} else {
gl.enableVertexAttribArray( this.colLoc );
gl.vertexAttribPointer(this.colLoc, 4, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.cofs);
}
}
if (is_lit && obj.vOffsets.nofs > 0) {
gl.enableVertexAttribArray( obj.normLoc );
gl.vertexAttribPointer(obj.normLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nofs);
}
if (has_texture || type === "text") {
gl.enableVertexAttribArray( obj.texLoc );
gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.tofs);
gl.activeTexture(gl.TEXTURE0);
gl.bindTexture(gl.TEXTURE_2D, obj.texture);
gl.uniform1i( obj.sampler, 0);
}
if (fixed_quads) {
gl.enableVertexAttribArray( obj.ofsLoc );
gl.vertexAttribPointer(obj.ofsLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.oofs);
}
if (typeof obj.userAttributes !== "undefined") {
for (attr in obj.userAttribSizes) {  // Not all attributes may have been used
gl.enableVertexAttribArray( obj.userAttribLocations[attr] );
gl.vertexAttribPointer( obj.userAttribLocations[attr], obj.userAttribSizes[attr],
gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.userAttribOffsets[attr]);
}
}
if (typeof obj.userUniforms !== "undefined") {
for (attr in obj.userUniformLocations) {
var loc = obj.userUniformLocations[attr];
if (loc !== null) {
var uniform = obj.userUniforms[attr];
if (typeof uniform.length === "undefined")
gl.uniform1f(loc, uniform);
else if (typeof uniform[0].length === "undefined") {
uniform = new Float32Array(uniform);
switch(uniform.length) {
case 2: gl.uniform2fv(loc, uniform); break;
case 3: gl.uniform3fv(loc, uniform); break;
case 4: gl.uniform4fv(loc, uniform); break;
default: console.warn("bad uniform length");
}
} else if (uniform.length == 4 && uniform[0].length == 4)
gl.uniformMatrix4fv(loc, false, new Float32Array(uniform.getAsArray()));
else
console.warn("unsupported uniform matrix");
}
}
}
for (pass = 0; pass < obj.passes; pass++) {
pmode = obj.pmode[pass];
if (pmode === "culled")
continue;
mode = fat_lines && (is_lines || pmode == "lines") ? "TRIANGLES" : this.mode4type[type];
if (depth_sort && pmode == "filled") {// Don't try depthsorting on wireframe or points
var faces = this.depthSort(obj),
nfaces = faces.length,
frowsize = Math.floor(obj.f[pass].length/nfaces);
if (type !== "spheres") {
var f = obj.index_uint ? new Uint32Array(obj.f[pass].length) : new Uint16Array(obj.f[pass].length);
for (i=0; i<nfaces; i++) {
for (j=0; j<frowsize; j++) {
f[frowsize*i + j] = obj.f[pass][frowsize*faces[i] + j];
}
}
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.DYNAMIC_DRAW);
}
}
if (is_twosided)
gl.uniform1i(obj.frontLoc, pass !== 0);
if (type !== "spheres") 
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);
if (type === "sprites" || type === "text" || type === "quads") {
count = count * 6/4;
} else if (type === "surface") {
count = obj.f[pass].length;
}
count = obj.f[pass].length;
if (!is_lines && pmode === "lines" && !fat_lines) {
mode = "LINES";
} else if (pmode === "points") {
mode = "POINTS";
}
if ((is_lines || pmode === "lines") && fat_lines) {
gl.enableVertexAttribArray(obj.pointLoc);
gl.vertexAttribPointer(obj.pointLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.pointofs);
gl.enableVertexAttribArray(obj.nextLoc );
gl.vertexAttribPointer(obj.nextLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nextofs);
gl.uniform1f(obj.aspectLoc, this.vp.width/this.vp.height);
gl.uniform1f(obj.lwdLoc, this.getMaterial(id, "lwd")/this.vp.height);
}
gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*obj.vOffsets.stride,  4*obj.vOffsets.vofs);
gl.drawElements(gl[mode], count, obj.index_uint ? gl.UNSIGNED_INT : gl.UNSIGNED_SHORT, 0);
}
};
/**
* Draw the background for a subscene
* @param { number } id - id of background object
* @param { number } subsceneid - id of subscene
*/
rglwidgetClass.prototype.drawBackground = function(id, subsceneid) {
var gl = this.gl || this.initGL(),
obj = this.getObj(id),
bg, i;
if (!obj.initialized)
this.initObj(id);
if (obj.colors.length) {
bg = obj.colors[0];
gl.clearColor(bg[0], bg[1], bg[2], bg[3]);
gl.depthMask(true);
gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
}
if (typeof obj.quad !== "undefined") {
this.prMatrix.makeIdentity();
this.mvMatrix.makeIdentity();
gl.disable(gl.BLEND);
gl.disable(gl.DEPTH_TEST);
gl.depthMask(false);
for (i=0; i < obj.quad.length; i++)
this.drawObj(obj.quad[i], subsceneid);
}
};
/**
* Draw a subscene
* @param { number } subsceneid - id of subscene
* @param { boolean } opaquePass - is this the opaque drawing pass?
*/
rglwidgetClass.prototype.drawSubscene = function(subsceneid, opaquePass) {
var gl = this.gl || this.initGL(),
sub = this.getObj(subsceneid),
objects = this.scene.objects,
subids = sub.objects,
subscene_has_faces = false,
subscene_needs_sorting = false,
flags, i, obj;
if (sub.par3d.skipRedraw)
return;
for (i=0; i < subids.length; i++) {
obj = objects[subids[i]];
flags = obj.flags;
if (typeof flags !== "undefined") {
subscene_has_faces |= (flags & this.f_is_lit)
& !(flags & this.f_fixed_quads);
obj.is_transparent = (flags & this.f_is_transparent) || obj.someHidden;
subscene_needs_sorting |= (flags & this.f_depth_sort) || obj.is_transparent;
}
}
this.setViewport(subsceneid);
if (typeof sub.backgroundId !== "undefined" && opaquePass)
this.drawBackground(sub.backgroundId, subsceneid);
if (subids.length) {
this.setprMatrix(subsceneid);
this.setmvMatrix(subsceneid);
if (subscene_has_faces) {
this.setnormMatrix(subsceneid);
if ((sub.flags & this.f_sprites_3d) &&
typeof sub.spriteNormmat === "undefined") {
sub.spriteNormmat = new CanvasMatrix4(this.normMatrix);
}
}
if (subscene_needs_sorting)
this.setprmvMatrix();
var clipids = sub.clipplanes;
if (typeof clipids === "undefined") {
console.warn("bad clipids");
}
if (clipids.length > 0) {
this.invMatrix = new CanvasMatrix4(this.mvMatrix);
this.invMatrix.invert();
for (i = 0; i < clipids.length; i++)
this.drawObj(clipids[i], subsceneid);
}
subids = sub.opaque.concat(sub.transparent);
if (opaquePass) {
gl.enable(gl.DEPTH_TEST);
gl.depthMask(true);
gl.disable(gl.BLEND);
for (i = 0; i < subids.length; i++) {
if (!this.getObj(subids[i]).is_transparent) 
this.drawObj(subids[i], subsceneid);
}
} else {
gl.depthMask(false);
gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA,
gl.ONE, gl.ONE);
gl.enable(gl.BLEND);
for (i = 0; i < subids.length; i++) {
if (this.getObj(subids[i]).is_transparent)
this.drawObj(subids[i], subsceneid);
}
}
subids = sub.subscenes;
for (i = 0; i < subids.length; i++) {
this.drawSubscene(subids[i], opaquePass);
}
}
};
/**
* Respond to brush change
*/
rglwidgetClass.prototype.selectionChanged = function() {
var i, j, k, id, subid = this.select.subscene, subscene,
objids, obj,
p1 = this.select.region.p1, p2 = this.select.region.p2,
filter, selection = [], handle, keys, xmin, x, xmax, ymin, y, ymax, z, v,
someHidden;
if (!subid)
return;
subscene = this.getObj(subid);
objids = subscene.objects;
filter = this.scene.crosstalk.filter;
this.setmvMatrix(subid);
this.setprMatrix(subid);
this.setprmvMatrix();
xmin = Math.min(p1.x, p2.x);
xmax = Math.max(p1.x, p2.x);
ymin = Math.min(p1.y, p2.y);
ymax = Math.max(p1.y, p2.y);
for (i = 0; i < objids.length; i++) {
id = objids[i];
j = this.scene.crosstalk.id.indexOf(id);
if (j >= 0) {
keys = this.scene.crosstalk.key[j];
obj = this.getObj(id);
someHidden = false;
for (k = 0; k < keys.length; k++) {
if (filter && filter.indexOf(keys[k]) < 0) {
someHidden = true;
continue;
}
v = [].concat(obj.vertices[k]).concat(1.0);
v = this.multVM(v, this.prmvMatrix);
x = v[0]/v[3];
y = v[1]/v[3];
z = v[2]/v[3];
if (xmin <= x && x <= xmax && ymin <= y && y <= ymax && -1.0 <= z && z <= 1.0) {
selection.push(keys[k]);
} else
someHidden = true;
}
obj.someHidden = someHidden && (filter || selection.length);
obj.initialized = false;
/* Who should we notify?  Only shared data in the current subscene, or everyone? */
if (!this.equalArrays(selection, this.scene.crosstalk.selection)) {
handle = this.scene.crosstalk.sel_handle[j];
handle.set(selection, {rglSubsceneId: this.select.subscene});
}
}
}
};
/**
* Respond to selection or filter change from crosstalk
* @param { Object } event - crosstalk event
* @param { boolean } filter - filter or selection?
*/
rglwidgetClass.prototype.selection = function(event, filter) {
var i, j, ids, obj, keys, crosstalk = this.scene.crosstalk,
selection, someHidden;
// Record the message and find out if this event makes some objects have mixed values:
crosstalk = this.scene.crosstalk;
if (filter) {
filter = crosstalk.filter = event.value;
selection = crosstalk.selection;
} else {  
selection = crosstalk.selection = event.value;
filter = crosstalk.filter;
}
ids = crosstalk.id;
for (i = 0; i < ids.length ; i++) {
obj = this.getObj(ids[i]);
obj.initialized = false;
keys = crosstalk.key[i];
someHidden = false;
for (j = 0; j < keys.length && !someHidden; j++) {
if ((filter && filter.indexOf(keys[j]) < 0) ||
(selection.length && selection.indexOf(keys[j]) < 0))
someHidden = true;
}
obj.someHidden = someHidden;
}
this.drawScene();
};
/**
* Clear the selection brush
* @param { number } except - Subscene that should ignore this request
*/
rglwidgetClass.prototype.clearBrush = function(except) {
if (this.select.subscene != except) {
this.select.state = "inactive";
this.delFromSubscene(this.scene.brushId, this.select.subscene);
}
this.drawScene();
};
/**
* Compute mouse coordinates relative to current canvas
* @returns { Object }
* @param { Object } event - event object from mouse click
*/
rglwidgetClass.prototype.relMouseCoords = function(event) {
var totalOffsetX = 0,
totalOffsetY = 0,
currentElement = this.canvas;
do {
totalOffsetX += currentElement.offsetLeft;
totalOffsetY += currentElement.offsetTop;
currentElement = currentElement.offsetParent;
}
while(currentElement);
var canvasX = event.pageX - totalOffsetX,
canvasY = event.pageY - totalOffsetY;
return {x:canvasX, y:canvasY};
};
/**
* Set mouse handlers for the scene
*/
rglwidgetClass.prototype.setMouseHandlers = function() {
var self = this, activeSubscene, handler,
handlers = {}, drag = 0;
handlers.rotBase = 0;
this.screenToVector = function(x, y) {
var viewport = this.getObj(activeSubscene).par3d.viewport,
width = viewport.width*this.canvas.width,
height = viewport.height*this.canvas.height,
radius = Math.max(width, height)/2.0,
cx = width/2.0,
cy = height/2.0,
px = (x-cx)/radius,
py = (y-cy)/radius,
plen = Math.sqrt(px*px+py*py);
if (plen > 1.e-6) {
px = px/plen;
py = py/plen;
}
var angle = (Math.SQRT2 - plen)/Math.SQRT2*Math.PI/2,
z = Math.sin(angle),
zlen = Math.sqrt(1.0 - z*z);
px = px * zlen;
py = py * zlen;
return [px, py, z];
};
handlers.trackballdown = function(x,y) {
var activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
i, l = activeModel.par3d.listeners;
handlers.rotBase = this.screenToVector(x, y);
this.saveMat = [];
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
}
};
handlers.trackballmove = function(x,y) {
var rotCurrent = this.screenToVector(x,y),
rotBase = handlers.rotBase,
dot = rotBase[0]*rotCurrent[0] +
rotBase[1]*rotCurrent[1] +
rotBase[2]*rotCurrent[2],
angle = Math.acos( dot/this.vlen(rotBase)/this.vlen(rotCurrent) )*180.0/Math.PI,
axis = this.xprod(rotBase, rotCurrent),
objects = this.scene.objects,
activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
l = activeModel.par3d.listeners,
i;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.userMatrix.load(objects[l[i]].saveMat);
activeSub.par3d.userMatrix.rotate(angle, axis[0], axis[1], axis[2]);
}
this.drawScene();
};
handlers.trackballend = 0;
this.clamp = function(x, lo, hi) {
return Math.max(lo, Math.min(x, hi));
};
this.screenToPolar = function(x,y) {
var viewport = this.getObj(activeSubscene).par3d.viewport,
width = viewport.width*this.canvas.width,
height = viewport.height*this.canvas.height,
r = Math.min(width, height)/2,
dx = this.clamp(x - width/2, -r, r),
dy = this.clamp(y - height/2, -r, r);
return [Math.asin(dx/r), Math.asin(-dy/r)];
};
handlers.polardown = function(x,y) {
var activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
i, l = activeModel.par3d.listeners;
handlers.dragBase = this.screenToPolar(x, y);
this.saveMat = [];
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
activeSub.camBase = [-Math.atan2(activeSub.saveMat.m13, activeSub.saveMat.m11),
Math.atan2(activeSub.saveMat.m32, activeSub.saveMat.m22)];
}
};
handlers.polarmove = function(x,y) {
var dragCurrent = this.screenToPolar(x,y),
activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
objects = this.scene.objects,
l = activeModel.par3d.listeners,
i, changepos = [];
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
for (j=0; j<2; j++)
changepos[j] = -(dragCurrent[j] - handlers.dragBase[j]);
activeSub.par3d.userMatrix.makeIdentity();
activeSub.par3d.userMatrix.rotate(changepos[0]*180/Math.PI, 0,-1,0);
activeSub.par3d.userMatrix.multRight(objects[l[i]].saveMat);
activeSub.par3d.userMatrix.rotate(changepos[1]*180/Math.PI, -1,0,0);
}
this.drawScene();
};
handlers.polarend = 0;
handlers.axisdown = function(x,y) {
handlers.rotBase = this.screenToVector(x, this.canvas.height/2);
var activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
i, l = activeModel.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
}
};
handlers.axismove = function(x,y) {
var rotCurrent = this.screenToVector(x, this.canvas.height/2),
rotBase = handlers.rotBase,
angle = (rotCurrent[0] - rotBase[0])*180/Math.PI,
rotMat = new CanvasMatrix4();
rotMat.rotate(angle, handlers.axis[0], handlers.axis[1], handlers.axis[2]);
var activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
i, l = activeModel.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.userMatrix.load(activeSub.saveMat);
activeSub.par3d.userMatrix.multLeft(rotMat);
}
this.drawScene();
};
handlers.axisend = 0;
handlers.y0zoom = 0;
handlers.zoom0 = 0;
handlers.zoomdown = function(x, y) {
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
handlers.y0zoom = y;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.zoom0 = Math.log(activeSub.par3d.zoom);
}
};
handlers.zoommove = function(x, y) {
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.zoom = Math.exp(activeSub.zoom0 + (y-handlers.y0zoom)/this.canvas.height);
}
this.drawScene();
};
handlers.zoomend = 0;
handlers.y0fov = 0;
handlers.fovdown = function(x, y) {
handlers.y0fov = y;
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.fov0 = activeSub.par3d.FOV;
}
};
handlers.fovmove = function(x, y) {
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.FOV = Math.max(1, Math.min(179, activeSub.fov0 +
180*(y-handlers.y0fov)/this.canvas.height));
}
this.drawScene();
};
handlers.fovend = 0;
handlers.selectingdown = function(x, y) {
var viewport = this.getObj(activeSubscene).par3d.viewport,
width = viewport.width*this.canvas.width,
height = viewport.height*this.canvas.height, 
p = {x: 2.0*x/width - 1.0, y: 2.0*y/height - 1.0};
this.select.region = {p1: p, p2: p};
if (this.select.subscene && this.select.subscene != activeSubscene)
this.delFromSubscene(this.scene.brushId, this.select.subscene);
this.select.subscene = activeSubscene;
this.addToSubscene(this.scene.brushId, activeSubscene);
this.select.state = "changing";
if (typeof this.scene.brushId !== "undefined")
this.getObj(this.scene.brushId).initialized = false;
this.drawScene();
};
handlers.selectingmove = function(x, y) {
var viewport = this.getObj(activeSubscene).par3d.viewport,
width = viewport.width*this.canvas.width,
height = viewport.height*this.canvas.height;
if (this.select.state === "inactive") 
return;
this.select.region.p2 = {x: 2.0*x/width - 1.0, y: 2.0*y/height - 1.0};
if (typeof this.scene.brushId !== "undefined")
this.getObj(this.scene.brushId).initialized = false;
this.drawScene();
};
handlers.selectingend = 0;
this.canvas.onmousedown = function ( ev ){
if (!ev.which) // Use w3c defns in preference to MS
switch (ev.button) {
case 0: ev.which = 1; break;
case 1:
case 4: ev.which = 2; break;
case 2: ev.which = 3;
}
drag = ["left", "middle", "right"][ev.which-1];
var coords = self.relMouseCoords(ev);
coords.y = self.canvas.height-coords.y;
activeSubscene = self.whichSubscene(coords);
var sub = self.getObj(activeSubscene), f;
handler = sub.par3d.mouseMode[drag];
switch (handler) {
case "xAxis":
handler = "axis";
handlers.axis = [1.0, 0.0, 0.0];
break;
case "yAxis":
handler = "axis";
handlers.axis = [0.0, 1.0, 0.0];
break;
case "zAxis":
handler = "axis";
handlers.axis = [0.0, 0.0, 1.0];
break;
}
f = handlers[handler + "down"];
if (f) {
coords = self.translateCoords(activeSubscene, coords);
f.call(self, coords.x, coords.y);
ev.preventDefault();
} else
console.warn("Mouse handler '" + handler + "' is not implemented.");
};
this.canvas.onmouseup = function ( ev ){
if ( drag === 0 ) return;
var f = handlers[handler + "end"];
if (f) {
f.call(self);
ev.preventDefault();
}
drag = 0;
};
this.canvas.onmouseout = this.canvas.onmouseup;
this.canvas.onmousemove = function ( ev ) {
if ( drag === 0 ) return;
var f = handlers[handler + "move"];
if (f) {
var coords = self.relMouseCoords(ev);
coords.y = self.canvas.height - coords.y;
coords = self.translateCoords(activeSubscene, coords);
f.call(self, coords.x, coords.y);
}
};
handlers.wheelHandler = function(ev) {
var del = 1.02, i;
if (ev.shiftKey) del = 1.002;
var ds = ((ev.detail || ev.wheelDelta) > 0) ? del : (1 / del);
if (typeof activeSubscene === "undefined")
activeSubscene = self.scene.rootSubscene;
var activeSub = self.getObj(activeSubscene),
activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = self.getObj(l[i]);
activeSub.par3d.zoom *= ds;
}
self.drawScene();
ev.preventDefault();
};
this.canvas.addEventListener("DOMMouseScroll", handlers.wheelHandler, false);
this.canvas.addEventListener("mousewheel", handlers.wheelHandler, false);
};
/**
* Find a particular subscene by inheritance
* @returns { number } id of subscene to use
* @param { number } subsceneid - child subscene
* @param { string } type - type of inheritance:  "projection" or "model"
*/
rglwidgetClass.prototype.useid = function(subsceneid, type) {
var sub = this.getObj(subsceneid);
if (sub.embeddings[type] === "inherit")
return(this.useid(sub.parent, type));
else
return subsceneid;
};
/**
* Check whether point is in viewport of subscene
* @returns {boolean}
* @param { Object } coords - screen coordinates of point
* @param { number } subsceneid - subscene to check
*/
rglwidgetClass.prototype.inViewport = function(coords, subsceneid) {
var viewport = this.getObj(subsceneid).par3d.viewport,
x0 = coords.x - viewport.x*this.canvas.width,
y0 = coords.y - viewport.y*this.canvas.height;
return 0 <= x0 && x0 <= viewport.width*this.canvas.width &&
0 <= y0 && y0 <= viewport.height*this.canvas.height;
};
/**
* Find which subscene contains a point
* @returns { number } subscene id
* @param { Object } coords - coordinates of point
*/
rglwidgetClass.prototype.whichSubscene = function(coords) {
var self = this,
recurse = function(subsceneid) {
var subscenes = self.getChildSubscenes(subsceneid), i, id;
for (i=0; i < subscenes.length; i++) {
id = recurse(subscenes[i]);
if (typeof(id) !== "undefined")
return(id);
}
if (self.inViewport(coords, subsceneid))
return(subsceneid);
else
return undefined;
},
rootid = this.scene.rootSubscene,
result = recurse(rootid);
if (typeof(result) === "undefined")
result = rootid;
return result;
};
/**
* Translate from window coordinates to viewport coordinates
* @returns { Object } translated coordinates
* @param { number } subsceneid - which subscene to use?
* @param { Object } coords - point to translate
*/
rglwidgetClass.prototype.translateCoords = function(subsceneid, coords) {
var viewport = this.getObj(subsceneid).par3d.viewport;
return {x: coords.x - viewport.x*this.canvas.width,
y: coords.y - viewport.y*this.canvas.height};
};
/**
* Initialize the sphere object
*/
rglwidgetClass.prototype.initSphere = function() {
var verts = this.scene.sphereVerts,
reuse = verts.reuse, result;
if (typeof reuse !== "undefined") {
var prev = document.getElementById(reuse).rglinstance.sphere;
result = {values: prev.values, vOffsets: prev.vOffsets, it: prev.it};
} else
result = {values: new Float32Array(this.flatten(this.cbind(this.transpose(verts.vb),
this.transpose(verts.texcoords)))),
it: new Uint16Array(this.flatten(this.transpose(verts.it))),
vOffsets: {vofs:0, cofs:-1, nofs:-1, radofs:-1, oofs:-1,
tofs:3, nextofs:-1, pointofs:-1, stride:5}};
result.sphereCount = result.it.length;
this.sphere = result;
};
/**
* Set the vertices in the selection box object
*/
rglwidgetClass.prototype.initSelection = function(id) {
if (typeof this.select.region === "undefined")
return;
var obj = this.getObj(id),
width = this.canvas.width,
height = this.canvas.height, 
p1 = this.select.region.p1,
p2 = this.select.region.p2;
obj.vertices = [[p1.x, p1.y, 0.0],
[p2.x, p1.y, 0.0],
[p2.x, p2.y, 0.0],
[p1.x, p2.y, 0.0],
[p1.x, p1.y, 0.0]];
};
/**
* Do the gl part of initializing the sphere
*/
rglwidgetClass.prototype.initSphereGL = function() {
var gl = this.gl || this.initGL(), sphere = this.sphere;
if (gl.isContextLost()) return;
sphere.buf = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, sphere.buf);
gl.bufferData(gl.ARRAY_BUFFER, sphere.values, gl.STATIC_DRAW);
sphere.ibuf = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, sphere.ibuf);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, sphere.it, gl.STATIC_DRAW);
return;
};
/**
* Initialize the DOM object
* @param { Object } el - the DOM object
* @param { Object } x - the scene data sent by JSON from R
*/
rglwidgetClass.prototype.initialize = function(el, x) {
this.textureCanvas = document.createElement("canvas");
this.textureCanvas.style.display = "block";
this.scene = x;
this.normMatrix = new CanvasMatrix4();
this.saveMat = {};
this.distance = null;
this.posLoc = 0;
this.colLoc = 1;
if (el) {
el.rglinstance = this;
this.el = el;
this.webGLoptions = el.rglinstance.scene.webGLoptions;
this.initCanvas();
}
};
/**
* Restart the WebGL canvas
*/
rglwidgetClass.prototype.restartCanvas = function() {
var newcanvas = document.createElement("canvas"),
self = this;
newcanvas.width = this.el.width;
newcanvas.height = this.el.height;
newcanvas.addEventListener("webglcontextrestored",
this.onContextRestored, false);
newcanvas.addEventListener("webglcontextlost",
this.onContextLost, false);
while (this.el.firstChild) {
this.el.removeChild(this.el.firstChild);
}
this.el.appendChild(newcanvas);
this.canvas = newcanvas;
this.setMouseHandlers();
if (this.gl) 
Object.keys(this.scene.objects).forEach(function(key){
self.getObj(parseInt(key, 10)).texture = undefined; 
});
this.gl = null;
};
/**
* Initialize the WebGL canvas
*/
rglwidgetClass.prototype.initCanvas = function() {
this.restartCanvas();
var objs = this.scene.objects,
self = this;
Object.keys(objs).forEach(function(key){
var id = parseInt(key, 10),
obj = self.getObj(id);
if (typeof obj.reuse !== "undefined")
self.copyObj(id, obj.reuse);
});
Object.keys(objs).forEach(function(key){
self.initSubscene(parseInt(key, 10));
});
this.setMouseHandlers();
this.initSphere();
this.onContextRestored = function(event) {
self.initGL();
self.drawScene();
};
this.onContextLost = function(event) {
if (!self.drawing)
this.gl = null;
event.preventDefault();
};
this.initGL0();
this.lazyLoadScene = function() {
if (typeof self.slide === "undefined")
self.slide = self.getSlide();
if (self.isInBrowserViewport()) {
if (!self.gl || self.gl.isContextLost())
self.initGL();
self.drawScene();
}
};
window.addEventListener("DOMContentLoaded", this.lazyLoadScene, false);
window.addEventListener("load", this.lazyLoadScene, false);
window.addEventListener("resize", this.lazyLoadScene, false);
window.addEventListener("scroll", this.lazyLoadScene, false);
this.slide = this.getSlide();
if (this.slide) {
if (typeof this.slide.rgl === "undefined")
this.slide.rgl = [this];
else
this.slide.rgl.push(this);
if (this.scene.context.rmarkdown) 
if (this.scene.context.rmarkdown === "ioslides_presentation") {
this.slide.setAttribute("slideenter", "this.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window);})");
} else if (this.scene.context.rmarkdown === "slidy_presentation") {
// This method would also work in ioslides, but it gets triggered
// something like 5 times per slide for every slide change, so
// you'd need a quicker function than lazyLoadScene.
var MutationObserver = window.MutationObserver || window.WebKitMutationObserver || window.MozMutationObserver,
observer = new MutationObserver(function(mutations) {
mutations.forEach(function(mutation) {
self.slide.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window); });});});
observer.observe(this.slide, { attributes: true, attributeFilter:["class"] });
}
}
};
/**
* Start the writeWebGL scene. This is only used by writeWebGL; rglwidget has
no debug element and does the drawing in rglwidget.js.
*/
rglwidgetClass.prototype.start = function() {
if (typeof this.prefix !== "undefined") {
this.debugelement = document.getElementById(this.prefix + "debug");
this.debug("");
}
this.drag = 0;
this.drawScene();
};
/**
* Display a debug message
* @param { string } msg - The message to display
* @param { Object } [img] - Image to insert before message
*/
rglwidgetClass.prototype.debug = function(msg, img) {
if (typeof this.debugelement !== "undefined" && this.debugelement !== null) {
this.debugelement.innerHTML = msg;
if (typeof img !== "undefined") {
this.debugelement.insertBefore(img, this.debugelement.firstChild);
}
} else if (msg !== "")
alert(msg);
};
/**
* Get the snapshot image of this scene
* @returns { Object } The img DOM element
*/
rglwidgetClass.prototype.getSnapshot = function() {
var img;
if (typeof this.scene.snapshot !== "undefined") {
img = document.createElement("img");
img.src = this.scene.snapshot;
img.alt = "Snapshot";
}
return img;
};
/**
* Initial test for WebGL
*/
rglwidgetClass.prototype.initGL0 = function() {
if (!window.WebGLRenderingContext){
alert("Your browser does not support WebGL. See http://get.webgl.org");
return;
}
};
/**
* If we are in an ioslides or slidy presentation, get the
* DOM element of the current slide
* @returns { Object }
*/
rglwidgetClass.prototype.getSlide = function() {
var result = this.el, done = false;
while (result && !done && this.scene.context.rmarkdown) {
switch(this.scene.context.rmarkdown) {
case "ioslides_presentation":
if (result.tagName === "SLIDE") return result;
break;
case "slidy_presentation":
if (result.tagName === "DIV" && result.classList.contains("slide"))
return result;
break;
default: return null;
}
result = result.parentElement;
}
return null;
};
/**
* Is this scene visible in the browser?
* @returns { boolean }
*/
rglwidgetClass.prototype.isInBrowserViewport = function() {
var rect = this.canvas.getBoundingClientRect(),
windHeight = (window.innerHeight || document.documentElement.clientHeight),
windWidth = (window.innerWidth || document.documentElement.clientWidth);
if (this.scene.context && this.scene.context.rmarkdown !== null) {
if (this.slide)
return (this.scene.context.rmarkdown === "ioslides_presentation" &&
this.slide.classList.contains("current")) ||
(this.scene.context.rmarkdown === "slidy_presentation" &&
!this.slide.classList.contains("hidden"));
}
return (
rect.top >= -windHeight &&
rect.left >= -windWidth &&
rect.bottom <= 2*windHeight &&
rect.right <= 2*windWidth);
};
/**
* Initialize WebGL
* @returns { Object } the WebGL context
*/
rglwidgetClass.prototype.initGL = function() {
var self = this;
if (this.gl) {
if (!this.drawing && this.gl.isContextLost())
this.restartCanvas();
else
return this.gl;
}
// if (!this.isInBrowserViewport()) return; Return what??? At this point we know this.gl is null.
this.canvas.addEventListener("webglcontextrestored",
this.onContextRestored, false);
this.canvas.addEventListener("webglcontextlost",
this.onContextLost, false);
this.gl = this.canvas.getContext("webgl", this.webGLoptions) ||
this.canvas.getContext("experimental-webgl", this.webGLoptions);
this.index_uint = this.gl.getExtension("OES_element_index_uint");
var save = this.startDrawing();
this.initSphereGL();
Object.keys(this.scene.objects).forEach(function(key){
self.initObj(parseInt(key, 10));
});
this.stopDrawing(save);
return this.gl;
};
/**
* Resize the display to match element
* @param { Object } el - DOM element to match
*/
rglwidgetClass.prototype.resize = function(el) {
this.canvas.width = el.width;
this.canvas.height = el.height;
};
/**
* Draw the whole scene
*/
rglwidgetClass.prototype.drawScene = function() {
var gl = this.gl || this.initGL(),
wasDrawing = this.startDrawing();
if (!wasDrawing) {
if (this.select.state !== "inactive")
this.selectionChanged();
gl.enable(gl.DEPTH_TEST);
gl.depthFunc(gl.LEQUAL);
gl.clearDepth(1.0);
gl.clearColor(1,1,1,1);
gl.depthMask(true); // Must be true before clearing depth buffer
gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
this.drawSubscene(this.scene.rootSubscene, true);
this.drawSubscene(this.scene.rootSubscene, false);
}
this.stopDrawing(wasDrawing);
};
/**
* Change the displayed subset
* @param { Object } el - Element of the control; not used.
* @param { Object } control - The subset control data.
*/
rglwidgetClass.prototype.subsetSetter = function(el, control) {
if (typeof control.subscenes === "undefined" ||
control.subscenes === null)
control.subscenes = this.scene.rootSubscene;
var value = Math.round(control.value),
subscenes = [].concat(control.subscenes),
fullset = [].concat(control.fullset),
i, j, entries, subsceneid,
adds = [], deletes = [],
ismissing = function(x) {
return fullset.indexOf(x) < 0;
},
tointeger = function(x) {
return parseInt(x, 10);
};
if (isNaN(value))
value = control.value = 0;
if (control.accumulate)
for (i=0; i <= value; i++)
adds = adds.concat(control.subsets[i]);
else
adds = adds.concat(control.subsets[value]);
deletes = fullset.filter(function(x) { return adds.indexOf(x) < 0; });
for (i = 0; i < subscenes.length; i++) {
subsceneid = subscenes[i];
if (typeof this.getObj(subsceneid) === "undefined")
this.alertOnce("typeof object is undefined");
for (j = 0; j < adds.length; j++)
this.addToSubscene(adds[j], subsceneid);
for (j = 0; j < deletes.length; j++)
this.delFromSubscene(deletes[j], subsceneid);
}
};
/**
* Change the requested property
* @param { Object } el - Element of the control; not used.
* @param { Object } control - The property setter control data.
*/
rglwidgetClass.prototype.propertySetter = function(el, control)  {
var value = control.value,
values = [].concat(control.values),
svals = [].concat(control.param),
direct = values[0] === null,
entries = [].concat(control.entries),
ncol = entries.length,
nrow = values.length/ncol,
properties = this.repeatToLen(control.properties, ncol),
objids = this.repeatToLen(control.objids, ncol),
property, objid = objids[0],
obj = this.getObj(objid),
propvals, i, v1, v2, p, entry, gl, needsBinding,
newprop, newid,
getPropvals = function() {
if (property === "userMatrix")
return obj.par3d.userMatrix.getAsArray();
else if (property === "scale" || property === "FOV" || property === "zoom")
return [].concat(obj.par3d[property]);
else
return [].concat(obj[property]);
};
putPropvals = function(newvals) {
if (newvals.length == 1)
newvals = newvals[0];
if (property === "userMatrix")
obj.par3d.userMatrix.load(newvals);
else if (property === "scale" || property === "FOV" || property === "zoom")
obj.par3d[property] = newvals;
else
obj[property] = newvals;
};
if (direct && typeof value === "undefined")
return;
if (control.interp) {
values = values.slice(0, ncol).concat(values).
concat(values.slice(ncol*(nrow-1), ncol*nrow));
svals = [-Infinity].concat(svals).concat(Infinity);
for (i = 1; i < svals.length; i++) {
if (value <= svals[i]) {
if (svals[i] === Infinity)
p = 1;
else
p = (svals[i] - value)/(svals[i] - svals[i-1]);
break;
}
}
} else if (!direct) {
value = Math.round(value);
}
for (j=0; j<entries.length; j++) {
entry = entries[j];
newprop = properties[j];
newid = objids[j];
if (newprop !== property || newid != objid) {
if (typeof property !== "undefined")
putPropvals(propvals);
property = newprop;
objid = newid;
obj = this.getObj(objid);
propvals = getPropvals();
}
if (control.interp) {
v1 = values[ncol*(i-1) + j];
v2 = values[ncol*i + j];
this.setElement(propvals, entry, p*v1 + (1-p)*v2);
} else if (!direct) {
this.setElement(propvals, entry, values[ncol*value + j]);
} else {
this.setElement(propvals, entry, value[j]);
}
}
putPropvals(propvals);
needsBinding = [];
for (j=0; j < entries.length; j++) {
if (properties[j] === "values" &&
needsBinding.indexOf(objids[j]) === -1) {
needsBinding.push(objids[j]);
}
}
for (j=0; j < needsBinding.length; j++) {
gl = this.gl || this.initGL();
obj = this.getObj(needsBinding[j]);
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW);
}
};
/**
* Change the requested vertices
* @param { Object } el - Element of the control; not used.
* @param { Object } control - The vertext setter control data.
*/
rglwidgetClass.prototype.vertexSetter = function(el, control)  {
var svals = [].concat(control.param),
j, k, p, a, propvals, stride, ofs, obj, entry,
attrib,
ofss    = {x:"vofs", y:"vofs", z:"vofs",
red:"cofs", green:"cofs", blue:"cofs",
alpha:"cofs", radii:"radofs",
nx:"nofs", ny:"nofs", nz:"nofs",
ox:"oofs", oy:"oofs", oz:"oofs",
ts:"tofs", tt:"tofs"},
pos     = {x:0, y:1, z:2,
red:0, green:1, blue:2,
alpha:3,radii:0,
nx:0, ny:1, nz:2,
ox:0, oy:1, oz:2,
ts:0, tt:1},
values = control.values,
direct = values === null,
ncol,
interp = control.interp,
vertices = [].concat(control.vertices),
attributes = [].concat(control.attributes),
value = control.value, newval, aliases, alias;
ncol = Math.max(vertices.length, attributes.length);
if (!ncol)
return;
vertices = this.repeatToLen(vertices, ncol);
attributes = this.repeatToLen(attributes, ncol);
if (direct)
interp = false;
/* JSON doesn't pass Infinity */
svals[0] = -Infinity;
svals[svals.length - 1] = Infinity;
for (j = 1; j < svals.length; j++) {
if (value <= svals[j]) {
if (interp) {
if (svals[j] === Infinity)
p = 1;
else
p = (svals[j] - value)/(svals[j] - svals[j-1]);
} else {
if (svals[j] - value > value - svals[j-1])
j = j - 1;
}
break;
}
}
obj = this.getObj(control.objid);
// First, make sure color attributes vary in original
if (typeof obj.vOffsets !== "undefined") {
varies = true;
for (k = 0; k < ncol; k++) {
attrib = attributes[k];
if (typeof attrib !== "undefined") {
ofs = obj.vOffsets[ofss[attrib]];
if (ofs < 0) {
switch(attrib) {
case "alpha":
case "red":
case "green":
case "blue":
obj.colors = [obj.colors[0], obj.colors[0]];
break;
}
varies = false;
}
}
}
if (!varies)
this.initObj(control.objid);
}
propvals = obj.values;
aliases = obj.alias;
if (typeof aliases === "undefined")
aliases = [];
for (k=0; k<ncol; k++) {
if (direct) {
newval = value;
} else if (interp) {
newval = p*values[j-1][k] + (1-p)*values[j][k];
} else {
newval = values[j][k];
}       
attrib = attributes[k];
vertex = vertices[k];
alias = aliases[vertex];
if (obj.type === "planes" || obj.type === "clipplanes") {
ofs = ["nx", "ny", "nz", "offset"].indexOf(attrib);
if (ofs >= 0) {
if (ofs < 3) {
if (obj.normals[vertex][ofs] != newval) {  // Assume no aliases here...
obj.normals[vertex][ofs] = newval;
obj.initialized = false;
}
} else {
if (obj.offsets[vertex][0] != newval) {
obj.offsets[vertex][0] = newval;
obj.initialized = false;
}
}
continue;
}
}
// Not a plane setting...
ofs = obj.vOffsets[ofss[attrib]];
if (ofs < 0)
this.alertOnce("Attribute '"+attrib+"' not found in object "+control.objid);
else {
stride = obj.vOffsets.stride;
ofs = ofs + pos[attrib];
entry = vertex*stride + ofs;
propvals[entry] = newval;
if (typeof alias !== "undefined")
for (a = 0; a < alias.length; a++)
propvals[alias[a]*stride + ofs] = newval;
}
}
if (typeof obj.buf !== "undefined") {
var gl = this.gl || this.initGL();
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, propvals, gl.STATIC_DRAW);
}
};
/**
* Change the requested vertex properties by age
* @param { Object } el - Element of the control; not used.
* @param { Object } control - The age setter control data.
*/
rglwidgetClass.prototype.ageSetter = function(el, control) {
var objids = [].concat(control.objids),
nobjs = objids.length,
time = control.value,
births = [].concat(control.births),
ages = [].concat(control.ages),
steps = births.length,
j = Array(steps),
p = Array(steps),
i, k, age, j0, propvals, stride, ofs, objid, obj,
attrib, dim, varies, alias, aliases, a, d,
attribs = ["colors", "alpha", "radii", "vertices",
"normals", "origins", "texcoords",
"x", "y", "z",
"red", "green", "blue"],
ofss    = ["cofs", "cofs", "radofs", "vofs",
"nofs", "oofs", "tofs",
"vofs", "vofs", "vofs",
"cofs", "cofs", "cofs"],
dims    = [3,1,1,3,
3,2,2,
1,1,1,
1,1,1],
pos     = [0,3,0,0,
0,0,0,
0,1,2,
0,1,2];
/* Infinity doesn't make it through JSON */
ages[0] = -Infinity;
ages[ages.length-1] = Infinity;
for (i = 0; i < steps; i++) {
if (births[i] !== null) {  // NA in R becomes null
age = time - births[i];
for (j0 = 1; age > ages[j0]; j0++);
if (ages[j0] == Infinity)
p[i] = 1;
else if (ages[j0] > ages[j0-1])
p[i] = (ages[j0] - age)/(ages[j0] - ages[j0-1]);
else
p[i] = 0;
j[i] = j0;
}
}
// First, make sure color attributes vary in original
for (l = 0; l < nobjs; l++) {
objid = objids[l];
obj = this.getObj(objid);
varies = true;
if (typeof obj.vOffsets === "undefined")
continue;
for (k = 0; k < attribs.length; k++) {
attrib = control[attribs[k]];
if (typeof attrib !== "undefined") {
ofs = obj.vOffsets[ofss[k]];
if (ofs < 0) {
switch(attribs[k]) {
case "colors":
case "alpha":
case "red":
case "green":
case "blue":
obj.colors = [obj.colors[0], obj.colors[0]];
break;
}
varies = false;
}
}
}
if (!varies)
this.initObj(objid);
}
for (l = 0; l < nobjs; l++) {
objid = objids[l];
obj = this.getObj(objid);
if (typeof obj.vOffsets === "undefined")
continue;
aliases = obj.alias;
if (typeof aliases === "undefined")
aliases = [];
propvals = obj.values;
stride = obj.vOffsets.stride;
for (k = 0; k < attribs.length; k++) {
attrib = control[attribs[k]];
if (typeof attrib !== "undefined") {
ofs = obj.vOffsets[ofss[k]];
if (ofs >= 0) {
dim = dims[k];
ofs = ofs + pos[k];
for (i = 0; i < steps; i++) {
alias = aliases[i];
if (births[i] !== null) {
for (d=0; d < dim; d++) {
propvals[i*stride + ofs + d] = p[i]*attrib[dim*(j[i]-1) + d] + (1-p[i])*attrib[dim*j[i] + d];
if (typeof alias !== "undefined")
for (a=0; a < alias.length; a++)
propvals[alias[a]*stride + ofs + d] = propvals[i*stride + ofs + d];
}
}
}
} else
this.alertOnce("\'"+attribs[k]+"\' property not found in object "+objid);
}
}
obj.values = propvals;
if (typeof obj.buf !== "undefined") {
gl = this.gl || this.initGL();
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW);
}
}
};
/**
* Bridge to old style control
* @param { Object } el - Element of the control; not used.
* @param { Object } control - The bridge control data.
*/
rglwidgetClass.prototype.oldBridge = function(el, control) {
var attrname, global = window[control.prefix + "rgl"];
if (global)
for (attrname in global)
this[attrname] = global[attrname];
window[control.prefix + "rgl"] = this;
};
/**
* Set up a player control
* @param { Object } el - The player control element
* @param { Object } control - The player data.
*/
rglwidgetClass.prototype.Player = function(el, control) {
var
self = this,
components = [].concat(control.components),
buttonLabels = [].concat(control.buttonLabels),
Tick = function() { /* "this" will be a timer */
var i,
nominal = this.value,
slider = this.Slider,
labels = this.outputLabels,
output = this.Output,
step;
if (typeof slider !== "undefined" && nominal != slider.value)
slider.value = nominal;
if (typeof output !== "undefined") {
step = Math.round((nominal - output.sliderMin)/output.sliderStep);
if (labels !== null) {
output.innerHTML = labels[step];
} else {
step = step*output.sliderStep + output.sliderMin;
output.innerHTML = step.toPrecision(output.outputPrecision);
}
}
for (i=0; i < this.actions.length; i++) {
this.actions[i].value = nominal;
}
self.applyControls(el, this.actions, false);
self.drawScene();
},
OnSliderInput = function() { /* "this" will be the slider */
this.rgltimer.value = Number(this.value);
this.rgltimer.Tick();
},
addSlider = function(min, max, step, value) {
var slider = document.createElement("input");
slider.type = "range";
slider.min = min;
slider.max = max;
slider.step = step;
slider.value = value;
slider.oninput = OnSliderInput;
slider.sliderActions = control.actions;
slider.sliderScene = this;
slider.className = "rgl-slider";
slider.id = el.id + "-slider";
el.rgltimer.Slider = slider;
slider.rgltimer = el.rgltimer;
el.appendChild(slider);
},
addLabel = function(labels, min, step, precision) {
var output = document.createElement("output");
output.sliderMin = min;
output.sliderStep = step;
output.outputPrecision = precision;
output.className = "rgl-label";
output.id = el.id + "-label";
el.rgltimer.Output = output;
el.rgltimer.outputLabels = labels;
el.appendChild(output);
},
addButton = function(which, label, active) {
var button = document.createElement("input"),
onclicks = {Reverse: function() { this.rgltimer.reverse();},
Play: function() { this.rgltimer.play();
this.value = this.rgltimer.enabled ? this.inactiveValue : this.activeValue; },
Slower: function() { this.rgltimer.slower(); },
Faster: function() { this.rgltimer.faster(); },
Reset: function() { this.rgltimer.reset(); },
Step:  function() { this.rgltimer.step(); }
};
button.rgltimer = el.rgltimer;
button.type = "button";
button.value = label;
button.activeValue = label;
button.inactiveValue = active;
if (which === "Play")
button.rgltimer.PlayButton = button;
button.onclick = onclicks[which];
button.className = "rgl-button";
button.id = el.id + "-" + which;
el.appendChild(button);
};
if (typeof control.reinit !== "undefined" && control.reinit !== null) {
control.actions.reinit = control.reinit;
}
el.rgltimer = new rgltimerClass(Tick, control.start, control.interval, control.stop,
control.step, control.value, control.rate, control.loop, control.actions);
for (var i=0; i < components.length; i++) {
switch(components[i]) {
case "Slider": addSlider(control.start, control.stop,
control.step, control.value);
break;
case "Label": addLabel(control.labels, control.start,
control.step, control.precision);
break;
default:
addButton(components[i], buttonLabels[i], control.pause);
}
}
el.rgltimer.Tick();
};
/**
* Apply all registered controls
* @param { Object } el - DOM element of the control
* @param { Object } x - List of actions to apply
* @param { boolean } [draw=true] - Whether to redraw after applying
*/
rglwidgetClass.prototype.applyControls = function(el, x, draw) {
var self = this, reinit = x.reinit, i, control, type;
for (i = 0; i < x.length; i++) {
control = x[i];
type = control.type;
self[type](el, control);
}
if (typeof reinit !== "undefined" && reinit !== null) {
reinit = [].concat(reinit);
for (i = 0; i < reinit.length; i++)
self.getObj(reinit[i]).initialized = false;
}
if (typeof draw === "undefined" || draw)
self.drawScene();
};
/**
* Handler for scene change
* @param { Object } message - What sort of scene change to do?
*/
rglwidgetClass.prototype.sceneChangeHandler = function(message) {
var self = document.getElementById(message.elementId).rglinstance,
objs = message.objects, mat = message.material,
root = message.rootSubscene,
initSubs = message.initSubscenes,
redraw = message.redrawScene,
skipRedraw = message.skipRedraw,
deletes, subs, allsubs = [], i,j;
if (typeof message.delete !== "undefined") {
deletes = [].concat(message.delete);
if (typeof message.delfromSubscenes !== "undefined")
subs = [].concat(message.delfromSubscenes);
else
subs = [];
for (i = 0; i < deletes.length; i++) {
for (j = 0; j < subs.length; j++) {
self.delFromSubscene(deletes[i], subs[j]);
}
delete self.scene.objects[deletes[i]];
}
}
if (typeof objs !== "undefined") {
Object.keys(objs).forEach(function(key){
key = parseInt(key, 10);
self.scene.objects[key] = objs[key];
self.initObj(key);
var obj = self.getObj(key),
subs = [].concat(obj.inSubscenes), k;
allsubs = allsubs.concat(subs);
for (k = 0; k < subs.length; k++)
self.addToSubscene(key, subs[k]);
});
}
if (typeof mat !== "undefined") {
self.scene.material = mat;
}
if (typeof root !== "undefined") {
self.scene.rootSubscene = root;
}
if (typeof initSubs !== "undefined")
allsubs = allsubs.concat(initSubs);
allsubs = self.unique(allsubs);
for (i = 0; i < allsubs.length; i++) {
self.initSubscene(allsubs[i]);
}
if (typeof skipRedraw !== "undefined") {
root = self.getObj(self.scene.rootSubscene);
root.par3d.skipRedraw = skipRedraw;
}
if (redraw)
self.drawScene();
};
/**
* Set mouse mode for a subscene
* @param { string } mode - name of mode
* @param { number } button - button number (1 to 3)
* @param { number } subscene - subscene id number
* @param { number } stayActive - if truthy, don't clear brush
*/
rglwidgetClass.prototype.setMouseMode = function(mode, button, subscene, stayActive) {
var sub = this.getObj(subscene),
which = ["left", "right", "middle"][button - 1];
if (!stayActive && sub.par3d.mouseMode[which] === "selecting")
this.clearBrush(null);
sub.par3d.mouseMode[which] = mode;
};
/**
* The class of an rgl timer object
* @class
*/
/**
* Construct an rgltimerClass object
* @constructor
* @param { function } Tick - action when timer fires
* @param { number } startTime - nominal start time in seconds
* @param { number } interval - seconds between updates
* @param { number } stopTime - nominal stop time in seconds
* @param { number } stepSize - nominal step size
* @param { number } value - current nominal time
* @param { number } rate - nominal units per second
* @param { string } loop - "none", "cycle" or "oscillate"
* @param { Object } actions - list of actions
*/
rgltimerClass = function(Tick, startTime, interval, stopTime, stepSize, value, rate, loop, actions) {
this.enabled = false;
this.timerId = 0;
/** nominal start time in seconds */
this.startTime = startTime;   
/** current nominal time */      
this.value = value;
/** seconds between updates */                 
this.interval = interval;
/** nominal stop time */           
this.stopTime = stopTime;
/** nominal step size */           
this.stepSize = stepSize;
/** nominal units per second */           
this.rate = rate;
/** "none", "cycle", or "oscillate" */                   
this.loop = loop;
/** real world start time */                   
this.realStart = undefined;
/** multiplier for fast-forward or reverse */         
this.multiplier = 1;                
this.actions = actions;
this.Tick = Tick;
};
/**
* Start playing timer object
*/
rgltimerClass.prototype.play = function() {
if (this.enabled) {
this.enabled = false;
window.clearInterval(this.timerId);
this.timerId = 0;
return;
}
var tick = function(self) {
var now = new Date();
self.value = self.multiplier*self.rate*(now - self.realStart)/1000 + self.startTime;
self.forceToRange();
if (typeof self.Tick !== "undefined") {
self.Tick(self.value);
}
};
this.realStart = new Date() - 1000*(this.value - this.startTime)/this.rate/this.multiplier;
this.timerId = window.setInterval(tick, 1000*this.interval, this);
this.enabled = true;
};
/**
* Force value into legal range
*/
rgltimerClass.prototype.forceToRange = function() {
if (this.value > this.stopTime + this.stepSize/2 || this.value < this.startTime - this.stepSize/2) {
if (!this.loop) {
this.reset();
} else {
var cycle = this.stopTime - this.startTime + this.stepSize,
newval = (this.value - this.startTime) % cycle + this.startTime;
if (newval < this.startTime) {
newval += cycle;
}
this.realStart += (this.value - newval)*1000/this.multiplier/this.rate;
this.value = newval;
}
}
};
/**
* Reset to start values
*/
rgltimerClass.prototype.reset = function() {
this.value = this.startTime;
this.newmultiplier(1);
if (typeof this.Tick !== "undefined") {
this.Tick(this.value);
}
if (this.enabled)
this.play();  /* really pause... */
if (typeof this.PlayButton !== "undefined")
this.PlayButton.value = "Play";
};
/**
* Increase the multiplier to play faster
*/
rgltimerClass.prototype.faster = function() {
this.newmultiplier(Math.SQRT2*this.multiplier);
};
/**
* Decrease the multiplier to play slower
*/
rgltimerClass.prototype.slower = function() {
this.newmultiplier(this.multiplier/Math.SQRT2);
};
/**
* Change sign of multiplier to reverse direction
*/
rgltimerClass.prototype.reverse = function() {
this.newmultiplier(-this.multiplier);
};
/**
* Set multiplier for play speed
* @param { number } newmult - new value
*/
rgltimerClass.prototype.newmultiplier = function(newmult) {
if (newmult != this.multiplier) {
this.realStart += 1000*(this.value - this.startTime)/this.rate*(1/this.multiplier - 1/newmult);
this.multiplier = newmult;
}
};
/**
* Take one step
*/
rgltimerClass.prototype.step = function() {
this.value += this.rate*this.multiplier;
this.forceToRange();
if (typeof this.Tick !== "undefined")
this.Tick(this.value);
};</script>

<script type="text/javascript">
var unnamed_chunk_3div = document.getElementById("unnamed_chunk_3div"),
unnamed_chunk_3rgl = new rglwidgetClass();
unnamed_chunk_3div.width = 673;
unnamed_chunk_3div.height = 481;
unnamed_chunk_3rgl.initialize(unnamed_chunk_3div,
{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":true,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false},"rootSubscene":1,"objects":{"7":{"id":7,"type":"spheres","material":{"fog":false},"vertices":[[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1]],"ignoreExtent":false,"flags":3},"8":{"id":8,"type":"spheres","material":{"fog":false},"vertices":[[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2]],"ignoreExtent":false,"flags":3},"9":{"id":9,"type":"lines","material":{"alpha":[0.7647059,0.7647059,0.227451,0.227451,0.1372549,0.1372549,0.172549,0.172549,0.1960784,0.1960784,0.4666667,0.4666667,0.2705882,0.2705882,0.1607843,0.1607843,0.1372549,0.1372549,0.1686275,0.1686275,0.1215686,0.1215686,0.1215686,0.1215686,0.1490196,0.1490196,0.1490196,0.1490196,1,1,0.1803922,0.1803922,0.2627451,0.2627451,0.1372549,0.1372549,0.1647059,0.1647059,0.1098039,0.1098039,0.1019608,0.1019608,0.1372549,0.1372549],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.774331,4.141822,1],[1.883792,4.15316,2],[2.463811,5.256637,1],[2.452941,5.286639,2],[2.463811,5.256637,1],[3.743898,4.498632,2],[3.629513,4.248709,1],[2.452941,5.286639,2],[3.629513,4.248709,1],[3.743898,4.498632,2],[3.75255,4.690575,1],[3.744239,4.586135,2],[3.75255,4.690575,1],[3.757945,4.492146,2],[3.753949,4.372228,1],[3.743898,4.498632,2],[3.86918,4.48238,1],[2.452941,5.286639,2],[3.86918,4.48238,1],[3.743898,4.498632,2],[3.86918,4.48238,1],[4.127694,6.442847,2],[4.124667,6.443393,1],[3.743898,4.498632,2],[4.124667,6.443393,1],[4.127694,6.442847,2],[4.379174,4.989833,1],[4.58303,5.394935,2],[4.585269,5.395478,1],[4.58303,5.394935,2],[4.800633,5.510449,1],[2.452941,5.286639,2],[4.800633,5.510449,1],[3.438908,4.194337,2],[4.800633,5.510449,1],[3.743898,4.498632,2],[4.800633,5.510449,1],[4.127694,6.442847,2],[9.244714,10.34131,1],[2.452941,5.286639,2],[9.244714,10.34131,1],[4.127694,6.442847,2],[9.244714,10.34131,1],[6.149204,10.51344,2]],"colors":[[0,0,0,0.7647059],[0,0,0,0.7647059],[0,0,0,0.227451],[0,0,0,0.227451],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.172549],[0,0,0,0.172549],[0,0,0,0.1960784],[0,0,0,0.1960784],[0,0,0,0.4666667],[0,0,0,0.4666667],[0,0,0,0.2705882],[0,0,0,0.2705882],[0,0,0,0.1607843],[0,0,0,0.1607843],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.1686275],[0,0,0,0.1686275],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,1],[0,0,0,1],[0,0,0,0.1803922],[0,0,0,0.1803922],[0,0,0,0.2627451],[0,0,0,0.2627451],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.1647059],[0,0,0,0.1647059],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.1019608],[0,0,0,0.1019608],[0,0,0,0.1372549],[0,0,0,0.1372549]],"centers":[[1.829062,4.147491,1.5],[2.458376,5.271638,1.5],[3.103855,4.877634,1.5],[3.041227,4.767674,1.5],[3.686706,4.373671,1.5],[3.748395,4.638355,1.5],[3.755248,4.591361,1.5],[3.748924,4.43543,1.5],[3.161061,4.88451,1.5],[3.806539,4.490506,1.5],[3.998437,5.462614,1.5],[3.934283,5.471012,1.5],[4.126181,6.44312,1.5],[4.481102,5.192384,1.5],[4.584149,5.395206,1.5],[3.626787,5.398544,1.5],[4.119771,4.852393,1.5],[4.272265,5.00454,1.5],[4.464163,5.976648,1.5],[5.848827,7.813972,1.5],[6.686204,8.392076,1.5],[7.696959,10.42737,1.5]],"ignoreExtent":false,"flags":16480},"10":{"id":10,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,1],[13,13,1]],"colors":[[0,0,0,1]],"centers":[[0,0,1],[13,13,1]],"ignoreExtent":false,"flags":64},"11":{"id":11,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,2],[13,13,2]],"colors":[[0,0,0,1]],"centers":[[0,0,2],[13,13,2]],"ignoreExtent":false,"flags":64},"12":{"id":12,"type":"spheres","material":{"fog":false},"vertices":[[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2]],"ignoreExtent":false,"flags":3},"13":{"id":13,"type":"spheres","material":{"fog":false},"vertices":[[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3]],"ignoreExtent":false,"flags":3},"14":{"id":14,"type":"lines","material":{"alpha":[0.7960784,0.7960784,0.3764706,0.3764706,0.2313726,0.2313726,0.2078431,0.2078431,0.1333333,0.1333333,0.5254902,0.5254902,0.1333333,0.1333333,0.1647059,0.1647059,1,1,0.1019608,0.1019608,0.3882353,0.3882353,0.6431373,0.6431373],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.883792,4.15316,2],[1.736609,4.125931,3],[2.452941,5.286639,2],[2.851068,5.54069,3],[3.743898,4.498632,2],[2.851068,5.54069,3],[3.743898,4.498632,2],[3.731448,4.250916,3],[3.743898,4.498632,2],[4.212198,6.406206,3],[3.744239,4.586135,2],[3.74173,4.685896,3],[3.744239,4.586135,2],[3.742332,4.486712,3],[3.757945,4.492146,2],[3.74173,4.685896,3],[3.757945,4.492146,2],[3.742332,4.486712,3],[4.127694,6.442847,2],[2.851068,5.54069,3],[4.127694,6.442847,2],[4.212198,6.406206,3],[4.58303,5.394935,2],[4.616407,5.433964,3]],"colors":[[0,0,0,0.7960784],[0,0,0,0.7960784],[0,0,0,0.3764706],[0,0,0,0.3764706],[0,0,0,0.2313726],[0,0,0,0.2313726],[0,0,0,0.2078431],[0,0,0,0.2078431],[0,0,0,0.1333333],[0,0,0,0.1333333],[0,0,0,0.5254902],[0,0,0,0.5254902],[0,0,0,0.1333333],[0,0,0,0.1333333],[0,0,0,0.1647059],[0,0,0,0.1647059],[0,0,0,1],[0,0,0,1],[0,0,0,0.1019608],[0,0,0,0.1019608],[0,0,0,0.3882353],[0,0,0,0.3882353],[0,0,0,0.6431373],[0,0,0,0.6431373]],"centers":[[1.8102,4.139545,2.5],[2.652005,5.413665,2.5],[3.297483,5.019661,2.5],[3.737673,4.374774,2.5],[3.978048,5.452419,2.5],[3.742984,4.636016,2.5],[3.743285,4.536423,2.5],[3.749837,4.589022,2.5],[3.750139,4.489429,2.5],[3.489381,5.991769,2.5],[4.169946,6.424526,2.5],[4.599719,5.41445,2.5]],"ignoreExtent":false,"flags":16480},"15":{"id":15,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,2],[13,13,2]],"colors":[[0,0,0,1]],"centers":[[0,0,2],[13,13,2]],"ignoreExtent":false,"flags":64},"16":{"id":16,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,3],[13,13,3]],"colors":[[0,0,0,1]],"centers":[[0,0,3],[13,13,3]],"ignoreExtent":false,"flags":64},"17":{"id":17,"type":"spheres","material":{"fog":false},"vertices":[[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3]],"ignoreExtent":false,"flags":3},"18":{"id":18,"type":"spheres","material":{"fog":false},"vertices":[[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4]],"ignoreExtent":false,"flags":3},"19":{"id":19,"type":"lines","material":{"alpha":[0.7647059,0.7647059,0.09803922,0.09803922,0.227451,0.227451,0.1215686,0.1215686,0.1764706,0.1764706,0.4901961,0.4901961,0.1098039,0.1098039,0.1098039,0.1098039,0.2196078,0.2196078,0.3490196,0.3490196,0.3686275,0.3686275,0.2431373,0.2431373,0.145098,0.145098,0.454902,0.454902,0.1098039,0.1098039,0.3176471,0.3176471,0.2156863,0.2156863,0.1098039,0.1098039,1,1,0.2901961,0.2901961],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.736609,4.125931,3],[1.732567,4.123106,4],[1.736609,4.125931,3],[4.118558,4.687138,4],[2.851068,5.54069,3],[4.062371,5.54427,4],[2.851068,5.54069,3],[4.13472,6.676558,4],[2.851068,5.54069,3],[4.187082,4.881758,4],[3.731448,4.250916,3],[3.76832,4.309572,4],[3.731448,4.250916,3],[4.13472,6.676558,4],[3.731448,4.250916,3],[4.187082,4.881758,4],[3.74173,4.685896,3],[3.756386,4.690742,4],[3.74173,4.685896,3],[4.118558,4.687138,4],[3.742332,4.486712,3],[3.756386,4.690742,4],[3.742332,4.486712,3],[4.118558,4.687138,4],[4.212198,6.406206,3],[3.76832,4.309572,4],[4.212198,6.406206,3],[4.13472,6.676558,4],[4.212198,6.406206,3],[4.187082,4.881758,4],[4.373624,5.07511,3],[3.76832,4.309572,4],[4.373624,5.07511,3],[4.13472,6.676558,4],[4.373624,5.07511,3],[4.187082,4.881758,4],[4.616407,5.433964,3],[4.687388,5.395462,4],[6.898338,10.81486,3],[8.20857,10.97004,4]],"colors":[[0,0,0,0.7647059],[0,0,0,0.7647059],[0,0,0,0.09803922],[0,0,0,0.09803922],[0,0,0,0.227451],[0,0,0,0.227451],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1764706],[0,0,0,0.1764706],[0,0,0,0.4901961],[0,0,0,0.4901961],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.2196078],[0,0,0,0.2196078],[0,0,0,0.3490196],[0,0,0,0.3490196],[0,0,0,0.3686275],[0,0,0,0.3686275],[0,0,0,0.2431373],[0,0,0,0.2431373],[0,0,0,0.145098],[0,0,0,0.145098],[0,0,0,0.454902],[0,0,0,0.454902],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.3176471],[0,0,0,0.3176471],[0,0,0,0.2156863],[0,0,0,0.2156863],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,1],[0,0,0,1],[0,0,0,0.2901961],[0,0,0,0.2901961]],"centers":[[1.734588,4.124518,3.5],[2.927584,4.406534,3.5],[3.45672,5.54248,3.5],[3.492894,6.108624,3.5],[3.519075,5.211224,3.5],[3.749884,4.280244,3.5],[3.933084,5.463737,3.5],[3.959265,4.566337,3.5],[3.749058,4.688319,3.5],[3.930144,4.686517,3.5],[3.749359,4.588727,3.5],[3.930445,4.586925,3.5],[3.990259,5.357889,3.5],[4.173459,6.541382,3.5],[4.19964,5.643982,3.5],[4.070972,4.692341,3.5],[4.254172,5.875834,3.5],[4.280353,4.978434,3.5],[4.651897,5.414713,3.5],[7.553453,10.89245,3.5]],"ignoreExtent":false,"flags":16480},"20":{"id":20,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,3],[13,13,3]],"colors":[[0,0,0,1]],"centers":[[0,0,3],[13,13,3]],"ignoreExtent":false,"flags":64},"21":{"id":21,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,4],[13,13,4]],"colors":[[0,0,0,1]],"centers":[[0,0,4],[13,13,4]],"ignoreExtent":false,"flags":64},"22":{"id":22,"type":"spheres","material":{"fog":false},"vertices":[[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4]],"ignoreExtent":false,"flags":3},"23":{"id":23,"type":"spheres","material":{"fog":false},"vertices":[[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5]],"ignoreExtent":false,"flags":3},"24":{"id":24,"type":"lines","material":{"alpha":[0.6509804,0.6509804,0.2784314,0.2784314,0.2588235,0.2588235,0.1686275,0.1686275,0.6078432,0.6078432,0.1098039,0.1098039,0.2352941,0.2352941,0.3254902,0.3254902,0.4352941,0.4352941,0.2431373,0.2431373,0.1490196,0.1490196,0.09803922,0.09803922,0.4823529,0.4823529,0.1058824,0.1058824,0.1294118,0.1294118,0.254902,0.254902,1,1,0.2862745,0.2862745],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.732567,4.123106,4],[1.741514,4.13105,5],[3.756386,4.690742,4],[3.792702,4.679107,5],[3.756386,4.690742,4],[3.793629,4.583196,5],[3.76832,4.309572,4],[3.942643,5.100069,5],[3.76832,4.309572,4],[4.106479,4.662616,5],[3.76832,4.309572,4],[4.234904,6.410672,5],[3.76832,4.309572,4],[4.259863,4.783735,5],[4.062371,5.54427,4],[3.436065,5.516401,5],[4.118558,4.687138,4],[3.792702,4.679107,5],[4.118558,4.687138,4],[3.793629,4.583196,5],[4.13472,6.676558,4],[3.942643,5.100069,5],[4.13472,6.676558,4],[4.106479,4.662616,5],[4.13472,6.676558,4],[4.234904,6.410672,5],[4.13472,6.676558,4],[4.259863,4.783735,5],[4.187082,4.881758,4],[3.436065,5.516401,5],[4.187082,4.881758,4],[3.942643,5.100069,5],[4.687388,5.395462,4],[4.659994,5.385197,5],[8.20857,10.97004,4],[7.315459,10.68141,5]],"colors":[[0,0,0,0.6509804],[0,0,0,0.6509804],[0,0,0,0.2784314],[0,0,0,0.2784314],[0,0,0,0.2588235],[0,0,0,0.2588235],[0,0,0,0.1686275],[0,0,0,0.1686275],[0,0,0,0.6078432],[0,0,0,0.6078432],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.2352941],[0,0,0,0.2352941],[0,0,0,0.3254902],[0,0,0,0.3254902],[0,0,0,0.4352941],[0,0,0,0.4352941],[0,0,0,0.2431373],[0,0,0,0.2431373],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.09803922],[0,0,0,0.09803922],[0,0,0,0.4823529],[0,0,0,0.4823529],[0,0,0,0.1058824],[0,0,0,0.1058824],[0,0,0,0.1294118],[0,0,0,0.1294118],[0,0,0,0.254902],[0,0,0,0.254902],[0,0,0,1],[0,0,0,1],[0,0,0,0.2862745],[0,0,0,0.2862745]],"centers":[[1.73704,4.127078,4.5],[3.774544,4.684925,4.5],[3.775007,4.636969,4.5],[3.855481,4.704821,4.5],[3.9374,4.486094,4.5],[4.001612,5.360122,4.5],[4.014091,4.546653,4.5],[3.749218,5.530335,4.5],[3.95563,4.683123,4.5],[3.956094,4.635167,4.5],[4.038682,5.888313,4.5],[4.1206,5.669587,4.5],[4.184813,6.543615,4.5],[4.197291,5.730146,4.5],[3.811574,5.19908,4.5],[4.064862,4.990913,4.5],[4.673691,5.390329,4.5],[7.762014,10.82572,4.5]],"ignoreExtent":false,"flags":16480},"25":{"id":25,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,4],[13,13,4]],"colors":[[0,0,0,1]],"centers":[[0,0,4],[13,13,4]],"ignoreExtent":false,"flags":64},"26":{"id":26,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,5],[13,13,5]],"colors":[[0,0,0,1]],"centers":[[0,0,5],[13,13,5]],"ignoreExtent":false,"flags":64},"27":{"id":27,"type":"spheres","material":{"fog":false},"vertices":[[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5]],"ignoreExtent":false,"flags":3},"28":{"id":28,"type":"spheres","material":{"fog":false},"vertices":[[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6]],"ignoreExtent":false,"flags":3},"29":{"id":29,"type":"lines","material":{"alpha":[1,1,0.1058824,0.1058824,0.8627451,0.8627451,0.1137255,0.1137255,0.1294118,0.1294118,0.7803922,0.7803922,0.6980392,0.6980392,0.1490196,0.1490196,0.4509804,0.4509804,0.2705882,0.2705882,0.282353,0.282353,0.9882353,0.9882353,0.2078431,0.2078431,0.172549,0.172549,0.2313726,0.2313726,0.3333333,0.3333333,0.4862745,0.4862745,0.1686275,0.1686275,0.1098039,0.1098039,0.7921569,0.7921569,0.5215687,0.5215687],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.741514,4.13105,5],[1.848939,4.123323,6],[1.741514,4.13105,5],[3.891402,4.609963,6],[3.436065,5.516401,5],[3.786937,5.357449,6],[3.436065,5.516401,5],[4.151063,6.782679,6],[3.792702,4.679107,5],[1.848939,4.123323,6],[3.792702,4.679107,5],[3.741845,4.624436,6],[3.792702,4.679107,5],[3.891402,4.609963,6],[3.793629,4.583196,5],[3.741845,4.624436,6],[3.793629,4.583196,5],[3.891402,4.609963,6],[3.942643,5.100069,5],[3.786937,5.357449,6],[3.942643,5.100069,5],[3.858304,4.5126,6],[4.106479,4.662616,5],[3.858304,4.5126,6],[4.106479,4.662616,5],[4.151063,6.782679,6],[4.234904,6.410672,5],[3.786937,5.357449,6],[4.234904,6.410672,5],[3.858304,4.5126,6],[4.234904,6.410672,5],[4.151063,6.782679,6],[4.259863,4.783735,5],[3.858304,4.5126,6],[4.259863,4.783735,5],[4.151063,6.782679,6],[4.659994,5.385197,5],[4.196352,4.797724,6],[4.659994,5.385197,5],[4.582591,5.389624,6],[7.315459,10.68141,5],[6.991272,9.86127,6]],"colors":[[0,0,0,1],[0,0,0,1],[0,0,0,0.1058824],[0,0,0,0.1058824],[0,0,0,0.8627451],[0,0,0,0.8627451],[0,0,0,0.1137255],[0,0,0,0.1137255],[0,0,0,0.1294118],[0,0,0,0.1294118],[0,0,0,0.7803922],[0,0,0,0.7803922],[0,0,0,0.6980392],[0,0,0,0.6980392],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.4509804],[0,0,0,0.4509804],[0,0,0,0.2705882],[0,0,0,0.2705882],[0,0,0,0.282353],[0,0,0,0.282353],[0,0,0,0.9882353],[0,0,0,0.9882353],[0,0,0,0.2078431],[0,0,0,0.2078431],[0,0,0,0.172549],[0,0,0,0.172549],[0,0,0,0.2313726],[0,0,0,0.2313726],[0,0,0,0.3333333],[0,0,0,0.3333333],[0,0,0,0.4862745],[0,0,0,0.4862745],[0,0,0,0.1686275],[0,0,0,0.1686275],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.7921569],[0,0,0,0.7921569],[0,0,0,0.5215687],[0,0,0,0.5215687]],"centers":[[1.795226,4.127186,5.5],[2.816458,4.370506,5.5],[3.611501,5.436925,5.5],[3.793564,6.14954,5.5],[2.820821,4.401215,5.5],[3.767274,4.651772,5.5],[3.842052,4.644535,5.5],[3.767737,4.603816,5.5],[3.842516,4.59658,5.5],[3.86479,5.228759,5.5],[3.900473,4.806334,5.5],[3.982391,4.587608,5.5],[4.128771,5.722648,5.5],[4.010921,5.88406,5.5],[4.046604,5.461636,5.5],[4.192984,6.596675,5.5],[4.059083,4.648168,5.5],[4.205463,5.783207,5.5],[4.428173,5.09146,5.5],[4.621292,5.38741,5.5],[7.153366,10.27134,5.5]],"ignoreExtent":false,"flags":16480},"30":{"id":30,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,5],[13,13,5]],"colors":[[0,0,0,1]],"centers":[[0,0,5],[13,13,5]],"ignoreExtent":false,"flags":64},"31":{"id":31,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,6],[13,13,6]],"colors":[[0,0,0,1]],"centers":[[0,0,6],[13,13,6]],"ignoreExtent":false,"flags":64},"32":{"id":32,"type":"spheres","material":{"fog":false},"vertices":[[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6],[6.991272,9.86127,6]],"ignoreExtent":false,"flags":3},"33":{"id":33,"type":"spheres","material":{"fog":false},"vertices":[[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[11.25184,11.91421,7]],"ignoreExtent":false,"flags":3},"34":{"id":34,"type":"lines","material":{"alpha":[0.5450981,0.5450981,0.1490196,0.1490196,0.4666667,0.4666667,0.1019608,0.1019608,0.1137255,0.1137255,0.6941177,0.6941177,0.3686275,0.3686275,0.3803922,0.3803922,0.1647059,0.1647059,0.3803922,0.3803922,0.145098,0.145098,0.3019608,0.3019608,0.1960784,0.1960784,0.6313726,0.6313726,0.5960785,0.5960785,0.1411765,0.1411765,0.4,0.4,0.1882353,0.1882353,0.2,0.2,0.3176471,0.3176471,1,1,0.2352941,0.2352941,0.2352941,0.2352941,0.5529412,0.5529412],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.848939,4.123323,6],[1.799058,4.126131,7],[1.848939,4.123323,6],[3.799114,4.707514,7],[3.215719,4.137128,6],[3.178236,4.099946,7],[3.215719,4.137128,6],[4.401873,5.017281,7],[3.741845,4.624436,6],[1.799058,4.126131,7],[3.741845,4.624436,6],[3.799114,4.707514,7],[3.741845,4.624436,6],[3.830415,4.611758,7],[3.786937,5.357449,6],[3.83895,5.744577,7],[3.786937,5.357449,6],[4.232057,6.68141,7],[3.786937,5.357449,6],[4.401873,5.017281,7],[3.858304,4.5126,6],[4.232057,6.68141,7],[3.858304,4.5126,6],[4.401873,5.017281,7],[3.891402,4.609963,6],[1.799058,4.126131,7],[3.891402,4.609963,6],[3.799114,4.707514,7],[3.891402,4.609963,6],[3.830415,4.611758,7],[4.151063,6.782679,6],[3.83895,5.744577,7],[4.151063,6.782679,6],[4.232057,6.68141,7],[4.151063,6.782679,6],[4.401873,5.017281,7],[4.196352,4.797724,6],[4.618914,5.398361,7],[4.196352,4.797724,6],[11.25184,11.91421,7],[4.582591,5.389624,6],[4.618914,5.398361,7],[4.692433,5.343936,6],[3.83895,5.744577,7],[4.692433,5.343936,6],[4.401873,5.017281,7],[6.991272,9.86127,6],[6.361327,9.695569,7]],"colors":[[0,0,0,0.5450981],[0,0,0,0.5450981],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.4666667],[0,0,0,0.4666667],[0,0,0,0.1019608],[0,0,0,0.1019608],[0,0,0,0.1137255],[0,0,0,0.1137255],[0,0,0,0.6941177],[0,0,0,0.6941177],[0,0,0,0.3686275],[0,0,0,0.3686275],[0,0,0,0.3803922],[0,0,0,0.3803922],[0,0,0,0.1647059],[0,0,0,0.1647059],[0,0,0,0.3803922],[0,0,0,0.3803922],[0,0,0,0.145098],[0,0,0,0.145098],[0,0,0,0.3019608],[0,0,0,0.3019608],[0,0,0,0.1960784],[0,0,0,0.1960784],[0,0,0,0.6313726],[0,0,0,0.6313726],[0,0,0,0.5960785],[0,0,0,0.5960785],[0,0,0,0.1411765],[0,0,0,0.1411765],[0,0,0,0.4],[0,0,0,0.4],[0,0,0,0.1882353],[0,0,0,0.1882353],[0,0,0,0.2],[0,0,0,0.2],[0,0,0,0.3176471],[0,0,0,0.3176471],[0,0,0,1],[0,0,0,1],[0,0,0,0.2352941],[0,0,0,0.2352941],[0,0,0,0.2352941],[0,0,0,0.2352941],[0,0,0,0.5529412],[0,0,0,0.5529412]],"centers":[[1.823999,4.124727,6.5],[2.824027,4.415419,6.5],[3.196977,4.118537,6.5],[3.808796,4.577205,6.5],[2.770452,4.375284,6.5],[3.770479,4.665975,6.5],[3.78613,4.618097,6.5],[3.812943,5.551013,6.5],[4.009497,6.019429,6.5],[4.094405,5.187365,6.5],[4.04518,5.597005,6.5],[4.130088,4.76494,6.5],[2.84523,4.368047,6.5],[3.845258,4.658739,6.5],[3.860909,4.610861,6.5],[3.995007,6.263628,6.5],[4.19156,6.732044,6.5],[4.276468,5.89998,6.5],[4.407633,5.098042,6.5],[7.724094,8.355966,6.5],[4.600752,5.393992,6.5],[4.265692,5.544257,6.5],[4.547153,5.180609,6.5],[6.676299,9.778419,6.5]],"ignoreExtent":false,"flags":16480},"35":{"id":35,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,6],[13,13,6]],"colors":[[0,0,0,1]],"centers":[[0,0,6],[13,13,6]],"ignoreExtent":false,"flags":64},"36":{"id":36,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,7],[13,13,7]],"colors":[[0,0,0,1]],"centers":[[0,0,7],[13,13,7]],"ignoreExtent":false,"flags":64},"37":{"id":37,"type":"spheres","material":{"fog":false},"vertices":[[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[1.799058,4.126131,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.178236,4.099946,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.799114,4.707514,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.830415,4.611758,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[3.83895,5.744577,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.232057,6.68141,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.401873,5.017281,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[4.618914,5.398361,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[6.361327,9.695569,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[8.640981,11.92112,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7],[11.25184,11.91421,7]],"ignoreExtent":false,"flags":3},"38":{"id":38,"type":"spheres","material":{"fog":false},"vertices":[[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.899821,10.08637,8]],"ignoreExtent":false,"flags":3},"39":{"id":39,"type":"lines","material":{"alpha":[1,1,0.1294118,0.1294118,0.7568628,0.7568628,0.3098039,0.3098039,0.3098039,0.3098039,0.8666667,0.8666667,0.454902,0.454902,0.3098039,0.3098039,0.1529412,0.1529412,0.1333333,0.1333333,0.145098,0.145098,0.1254902,0.1254902,0.427451,0.427451,0.254902,0.254902,0.6901961,0.6901961,0.2941177,0.2941177,0.2039216,0.2039216,0.9058824,0.9058824,0.5607843,0.5607843],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.799058,4.126131,7],[2.098062,4.124048,8],[3.178236,4.099946,7],[3.609442,4.335504,8],[3.799114,4.707514,7],[3.769995,4.680377,8],[3.799114,4.707514,7],[3.854961,4.608563,8],[3.830415,4.611758,7],[3.769995,4.680377,8],[3.830415,4.611758,7],[3.854961,4.608563,8],[3.83895,5.744577,7],[3.485695,5.503438,8],[3.83895,5.744577,7],[3.795478,4.508286,8],[3.83895,5.744577,7],[4.251893,6.481557,8],[4.232057,6.68141,7],[3.485695,5.503438,8],[4.232057,6.68141,7],[3.795478,4.508286,8],[4.232057,6.68141,7],[3.820303,4.513925,8],[4.232057,6.68141,7],[4.251893,6.481557,8],[4.401873,5.017281,7],[3.485695,5.503438,8],[4.401873,5.017281,7],[3.795478,4.508286,8],[4.401873,5.017281,7],[3.820303,4.513925,8],[4.401873,5.017281,7],[4.251893,6.481557,8],[4.618914,5.398361,7],[4.641607,5.385232,8],[6.361327,9.695569,7],[4.899821,10.08637,8]],"colors":[[0,0,0,1],[0,0,0,1],[0,0,0,0.1294118],[0,0,0,0.1294118],[0,0,0,0.7568628],[0,0,0,0.7568628],[0,0,0,0.3098039],[0,0,0,0.3098039],[0,0,0,0.3098039],[0,0,0,0.3098039],[0,0,0,0.8666667],[0,0,0,0.8666667],[0,0,0,0.454902],[0,0,0,0.454902],[0,0,0,0.3098039],[0,0,0,0.3098039],[0,0,0,0.1529412],[0,0,0,0.1529412],[0,0,0,0.1333333],[0,0,0,0.1333333],[0,0,0,0.145098],[0,0,0,0.145098],[0,0,0,0.1254902],[0,0,0,0.1254902],[0,0,0,0.427451],[0,0,0,0.427451],[0,0,0,0.254902],[0,0,0,0.254902],[0,0,0,0.6901961],[0,0,0,0.6901961],[0,0,0,0.2941177],[0,0,0,0.2941177],[0,0,0,0.2039216],[0,0,0,0.2039216],[0,0,0,0.9058824],[0,0,0,0.9058824],[0,0,0,0.5607843],[0,0,0,0.5607843]],"centers":[[1.94856,4.12509,7.5],[3.393839,4.217725,7.5],[3.784554,4.693945,7.5],[3.827037,4.658038,7.5],[3.800205,4.646068,7.5],[3.842688,4.610161,7.5],[3.662323,5.624008,7.5],[3.817214,5.126431,7.5],[4.045422,6.113067,7.5],[3.858876,6.092424,7.5],[4.013768,5.594848,7.5],[4.02618,5.597667,7.5],[4.241975,6.581483,7.5],[3.943784,5.260359,7.5],[4.098676,4.762783,7.5],[4.111088,4.765603,7.5],[4.326883,5.749419,7.5],[4.63026,5.391797,7.5],[5.630574,9.890968,7.5]],"ignoreExtent":false,"flags":16480},"40":{"id":40,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,7],[13,13,7]],"colors":[[0,0,0,1]],"centers":[[0,0,7],[13,13,7]],"ignoreExtent":false,"flags":64},"41":{"id":41,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,8],[13,13,8]],"colors":[[0,0,0,1]],"centers":[[0,0,8],[13,13,8]],"ignoreExtent":false,"flags":64},"42":{"id":42,"type":"spheres","material":{"fog":false},"vertices":[[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[2.098062,4.124048,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.485695,5.503438,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.609442,4.335504,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.769995,4.680377,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.795478,4.508286,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.820303,4.513925,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[3.854961,4.608563,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.231977,4.828679,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.251893,6.481557,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.641607,5.385232,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8],[4.899821,10.08637,8]],"ignoreExtent":false,"flags":3},"43":{"id":43,"type":"spheres","material":{"fog":false},"vertices":[[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[9.168148,9.997303,9]],"ignoreExtent":false,"flags":3},"44":{"id":44,"type":"lines","material":{"alpha":[0.8156863,0.8156863,0.172549,0.172549,0.7294118,0.7294118,0.1372549,0.1372549,0.1333333,0.1333333,0.3843137,0.3843137,0.1529412,0.1529412,0.3058824,0.3058824,0.2627451,0.2627451,0.1568628,0.1568628,0.572549,0.572549,0.1490196,0.1490196,0.172549,0.172549,1,1,0.1490196,0.1490196,0.1568628,0.1568628,0.8156863,0.8156863,0.7215686,0.7215686,0.3607843,0.3607843],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[2.098062,4.124048,8],[1.751192,4.241534,9],[2.098062,4.124048,8],[3.74171,4.627586,9],[3.485695,5.503438,8],[3.031783,5.587906,9],[3.485695,5.503438,8],[4.150021,6.418077,9],[3.609442,4.335504,8],[3.580488,4.348433,9],[3.769995,4.680377,8],[3.74171,4.627586,9],[3.769995,4.680377,8],[3.747586,4.596096,9],[3.795478,4.508286,8],[3.031783,5.587906,9],[3.795478,4.508286,8],[3.747101,4.362153,9],[3.795478,4.508286,8],[4.150021,6.418077,9],[3.820303,4.513925,8],[3.747101,4.362153,9],[3.820303,4.513925,8],[4.150021,6.418077,9],[3.854961,4.608563,8],[3.74171,4.627586,9],[3.854961,4.608563,8],[3.747586,4.596096,9],[4.251893,6.481557,8],[3.031783,5.587906,9],[4.251893,6.481557,8],[3.747101,4.362153,9],[4.251893,6.481557,8],[4.150021,6.418077,9],[4.641607,5.385232,8],[4.613615,5.47854,9],[4.899821,10.08637,8],[6.926564,9.811538,9]],"colors":[[0,0,0,0.8156863],[0,0,0,0.8156863],[0,0,0,0.172549],[0,0,0,0.172549],[0,0,0,0.7294118],[0,0,0,0.7294118],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.1333333],[0,0,0,0.1333333],[0,0,0,0.3843137],[0,0,0,0.3843137],[0,0,0,0.1529412],[0,0,0,0.1529412],[0,0,0,0.3058824],[0,0,0,0.3058824],[0,0,0,0.2627451],[0,0,0,0.2627451],[0,0,0,0.1568628],[0,0,0,0.1568628],[0,0,0,0.572549],[0,0,0,0.572549],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.172549],[0,0,0,0.172549],[0,0,0,1],[0,0,0,1],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.1568628],[0,0,0,0.1568628],[0,0,0,0.8156863],[0,0,0,0.8156863],[0,0,0,0.7215686],[0,0,0,0.7215686],[0,0,0,0.3607843],[0,0,0,0.3607843]],"centers":[[1.924627,4.182791,8.5],[2.919886,4.375817,8.5],[3.258739,5.545672,8.5],[3.817858,5.960757,8.5],[3.594965,4.341968,8.5],[3.755853,4.653981,8.5],[3.758791,4.638236,8.5],[3.41363,5.048096,8.5],[3.771289,4.435219,8.5],[3.97275,5.463181,8.5],[3.783702,4.438039,8.5],[3.985162,5.466001,8.5],[3.798336,4.618074,8.5],[3.801274,4.602329,8.5],[3.641838,6.034731,8.5],[3.999497,5.421855,8.5],[4.200957,6.449817,8.5],[4.627611,5.431887,8.5],[5.913192,9.948952,8.5]],"ignoreExtent":false,"flags":16480},"45":{"id":45,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,8],[13,13,8]],"colors":[[0,0,0,1]],"centers":[[0,0,8],[13,13,8]],"ignoreExtent":false,"flags":64},"46":{"id":46,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,9],[13,13,9]],"colors":[[0,0,0,1]],"centers":[[0,0,9],[13,13,9]],"ignoreExtent":false,"flags":64},"47":{"id":47,"type":"spheres","material":{"fog":false},"vertices":[[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[1.751192,4.241534,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.031783,5.587906,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.580488,4.348433,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.74171,4.627586,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747101,4.362153,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[3.747586,4.596096,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.150021,6.418077,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[4.613615,5.47854,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.830553,7.336931,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.887665,7.387847,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[6.926564,9.811538,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9],[9.168148,9.997303,9]],"ignoreExtent":false,"flags":3},"48":{"id":48,"type":"spheres","material":{"fog":false},"vertices":[[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10],[2.005179,4.124917,10],[3.230935,5.382741,10],[3.410397,4.280388,10],[3.661672,4.601715,10],[3.746546,4.737925,10],[3.749313,4.606184,10],[3.838845,4.627647,10],[4.156162,4.860458,10],[4.167439,6.47921,10],[4.300492,5.277694,10],[4.795944,5.38783,10],[8.812709,11.44327,10],[8.825877,9.416516,10],[9.75108,10.26047,10]],"ignoreExtent":false,"flags":3},"49":{"id":49,"type":"lines","material":{"alpha":[0.3921569,0.3921569,0.1058824,0.1058824,0.4705882,0.4705882,0.2196078,0.2196078,0.3568628,0.3568628,0.1294118,0.1294118,0.5176471,0.5176471,0.1372549,0.1372549,0.2470588,0.2470588,0.2078431,0.2078431,0.1019608,0.1019608,0.1294118,0.1294118,1,1,0.09803922,0.09803922,0.1098039,0.1098039,0.6078432,0.6078432,0.5647059,0.5647059,0.145098,0.145098,0.1215686,0.1215686,0.2627451,0.2627451,0.5686275,0.5686275],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.751192,4.241534,9],[2.005179,4.124917,10],[1.751192,4.241534,9],[3.749313,4.606184,10],[3.031783,5.587906,9],[3.230935,5.382741,10],[3.031783,5.587906,9],[3.746546,4.737925,10],[3.580488,4.348433,9],[3.661672,4.601715,10],[3.74171,4.627586,9],[2.005179,4.124917,10],[3.74171,4.627586,9],[3.749313,4.606184,10],[3.74171,4.627586,9],[3.838845,4.627647,10],[3.747101,4.362153,9],[3.410397,4.280388,10],[3.747101,4.362153,9],[3.746546,4.737925,10],[3.747101,4.362153,9],[4.167439,6.47921,10],[3.747586,4.596096,9],[3.749313,4.606184,10],[3.747586,4.596096,9],[3.838845,4.627647,10],[4.150021,6.418077,9],[3.230935,5.382741,10],[4.150021,6.418077,9],[3.746546,4.737925,10],[4.150021,6.418077,9],[4.167439,6.47921,10],[4.613615,5.47854,9],[4.795944,5.38783,10],[6.830553,7.336931,9],[3.661672,4.601715,10],[6.887665,7.387847,9],[3.661672,4.601715,10],[6.926564,9.811538,9],[9.75108,10.26047,10],[9.168148,9.997303,9],[8.825877,9.416516,10]],"colors":[[0,0,0,0.3921569],[0,0,0,0.3921569],[0,0,0,0.1058824],[0,0,0,0.1058824],[0,0,0,0.4705882],[0,0,0,0.4705882],[0,0,0,0.2196078],[0,0,0,0.2196078],[0,0,0,0.3568628],[0,0,0,0.3568628],[0,0,0,0.1294118],[0,0,0,0.1294118],[0,0,0,0.5176471],[0,0,0,0.5176471],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.2470588],[0,0,0,0.2470588],[0,0,0,0.2078431],[0,0,0,0.2078431],[0,0,0,0.1019608],[0,0,0,0.1019608],[0,0,0,0.1294118],[0,0,0,0.1294118],[0,0,0,1],[0,0,0,1],[0,0,0,0.09803922],[0,0,0,0.09803922],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.6078432],[0,0,0,0.6078432],[0,0,0,0.5647059],[0,0,0,0.5647059],[0,0,0,0.145098],[0,0,0,0.145098],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.2627451],[0,0,0,0.2627451],[0,0,0,0.5686275],[0,0,0,0.5686275]],"centers":[[1.878186,4.183225,9.5],[2.750253,4.423859,9.5],[3.131359,5.485324,9.5],[3.389164,5.162915,9.5],[3.62108,4.475074,9.5],[2.873445,4.376251,9.5],[3.745512,4.616885,9.5],[3.790277,4.627617,9.5],[3.578749,4.32127,9.5],[3.746823,4.550038,9.5],[3.95727,5.420681,9.5],[3.74845,4.60114,9.5],[3.793216,4.611872,9.5],[3.690478,5.900409,9.5],[3.948284,5.578001,9.5],[4.15873,6.448644,9.5],[4.70478,5.433186,9.5],[5.246112,5.969323,9.5],[5.274669,5.994781,9.5],[8.338821,10.03601,9.5],[8.997013,9.706909,9.5]],"ignoreExtent":false,"flags":16480},"50":{"id":50,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,9],[13,13,9]],"colors":[[0,0,0,1]],"centers":[[0,0,9],[13,13,9]],"ignoreExtent":false,"flags":64},"51":{"id":51,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,10],[13,13,10]],"colors":[[0,0,0,1]],"centers":[[0,0,10],[13,13,10]],"ignoreExtent":false,"flags":64},"52":{"id":52,"type":"lines","material":{"lit":false,"fog":false},"vertices":[[0,-0.195,10.238],[12,-0.195,10.238],[0,-0.195,10.238],[0,-0.52975,10.4749],[2,-0.195,10.238],[2,-0.52975,10.4749],[4,-0.195,10.238],[4,-0.52975,10.4749],[6,-0.195,10.238],[6,-0.52975,10.4749],[8,-0.195,10.238],[8,-0.52975,10.4749],[10,-0.195,10.238],[10,-0.52975,10.4749],[12,-0.195,10.238],[12,-0.52975,10.4749]],"colors":[[0,0,0,1]],"centers":[[6,-0.195,10.238],[0,-0.362375,10.35645],[2,-0.362375,10.35645],[4,-0.362375,10.35645],[6,-0.362375,10.35645],[8,-0.362375,10.35645],[10,-0.362375,10.35645],[12,-0.362375,10.35645]],"ignoreExtent":true,"flags":64},"53":{"id":53,"type":"text","material":{"lit":false,"fog":false},"vertices":[[0,-1.19925,10.9487],[2,-1.19925,10.9487],[4,-1.19925,10.9487],[6,-1.19925,10.9487],[8,-1.19925,10.9487],[10,-1.19925,10.9487],[12,-1.19925,10.9487]],"colors":[[0,0,0,1]],"texts":[[" 0"],[" 2"],[" 4"],[" 6"],[" 8"],["10"],["12"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[0,-1.19925,10.9487],[2,-1.19925,10.9487],[4,-1.19925,10.9487],[6,-1.19925,10.9487],[8,-1.19925,10.9487],[10,-1.19925,10.9487],[12,-1.19925,10.9487]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"54":{"id":54,"type":"lines","material":{"lit":false,"fog":false},"vertices":[[-0.195,0,0.762],[-0.195,12,0.762],[-0.195,0,0.762],[-0.52975,0,0.5250999],[-0.195,2,0.762],[-0.52975,2,0.5250999],[-0.195,4,0.762],[-0.52975,4,0.5250999],[-0.195,6,0.762],[-0.52975,6,0.5250999],[-0.195,8,0.762],[-0.52975,8,0.5250999],[-0.195,10,0.762],[-0.52975,10,0.5250999],[-0.195,12,0.762],[-0.52975,12,0.5250999]],"colors":[[0,0,0,1]],"centers":[[-0.195,6,0.762],[-0.362375,0,0.6435499],[-0.362375,2,0.6435499],[-0.362375,4,0.6435499],[-0.362375,6,0.6435499],[-0.362375,8,0.6435499],[-0.362375,10,0.6435499],[-0.362375,12,0.6435499]],"ignoreExtent":true,"flags":64},"55":{"id":55,"type":"text","material":{"lit":false,"fog":false},"vertices":[[-1.19925,0,0.05129994],[-1.19925,2,0.05129994],[-1.19925,4,0.05129994],[-1.19925,6,0.05129994],[-1.19925,8,0.05129994],[-1.19925,10,0.05129994],[-1.19925,12,0.05129994]],"colors":[[0,0,0,1]],"texts":[[" 0"],[" 2"],[" 4"],[" 6"],[" 8"],["10"],["12"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-1.19925,0,0.05129994],[-1.19925,2,0.05129994],[-1.19925,4,0.05129994],[-1.19925,6,0.05129994],[-1.19925,8,0.05129994],[-1.19925,10,0.05129994],[-1.19925,12,0.05129994]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"56":{"id":56,"type":"lines","material":{"lit":false,"fog":false},"vertices":[[-0.195,-0.195,2],[-0.195,-0.195,10],[-0.195,-0.195,2],[-0.52975,-0.52975,2],[-0.195,-0.195,4],[-0.52975,-0.52975,4],[-0.195,-0.195,6],[-0.52975,-0.52975,6],[-0.195,-0.195,8],[-0.52975,-0.52975,8],[-0.195,-0.195,10],[-0.52975,-0.52975,10]],"colors":[[0,0,0,1]],"centers":[[-0.195,-0.195,6],[-0.362375,-0.362375,2],[-0.362375,-0.362375,4],[-0.362375,-0.362375,6],[-0.362375,-0.362375,8],[-0.362375,-0.362375,10]],"ignoreExtent":true,"flags":64},"57":{"id":57,"type":"text","material":{"lit":false,"fog":false},"vertices":[[-1.19925,-1.19925,2],[-1.19925,-1.19925,4],[-1.19925,-1.19925,6],[-1.19925,-1.19925,8],[-1.19925,-1.19925,10]],"colors":[[0,0,0,1]],"texts":[[" 2"],[" 4"],[" 6"],[" 8"],["10"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-1.19925,-1.19925,2],[-1.19925,-1.19925,4],[-1.19925,-1.19925,6],[-1.19925,-1.19925,8],[-1.19925,-1.19925,10]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"58":{"id":58,"type":"text","material":{"lit":false},"vertices":[[-2,-2,5]],"colors":[[0,0,0,1]],"texts":[["Time"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-2,-2,5]],"family":[["sans"]],"font":[[1]],"ignoreExtent":false,"flags":2064},"59":{"id":59,"type":"text","material":{"lit":false},"vertices":[[-4,6,-4]],"colors":[[0,0,0,1]],"texts":[["Death"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-4,6,-4]],"family":[["sans"]],"font":[[1]],"ignoreExtent":false,"flags":2064},"60":{"id":60,"type":"text","material":{"lit":false},"vertices":[[6,-2,13]],"colors":[[0,0,0,1]],"texts":[["Birth"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[6,-2,13]],"family":[["sans"]],"font":[[1]],"ignoreExtent":false,"flags":2064},"5":{"id":5,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"4":{"id":4,"type":"background","material":{},"colors":[[0.2980392,0.2980392,0.2980392,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"6":{"id":6,"type":"background","material":{"lit":false,"back":"lines","fog":false},"colors":[[1,1,1,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"1":{"id":1,"type":"subscene","par3d":{"antialias":0,"FOV":30,"ignoreExtent":false,"listeners":1,"mouseMode":{"left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,54.74337],"modelMatrix":[[0.9105204,0.009738045,0.41335,-6.010976],[0.01078209,0.9988235,-0.0472818,-5.32928],[-0.413324,0.04750787,0.9093437,-57.23676],[0,0,0,1]],"projMatrix":[[3.808215,0,0,0],[0,5.331501,0,0],[0,0,-3.863703,-197.3435],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[0.9105204,0.009738045,0.41335,0],[0.01078209,0.9988235,-0.0472818,0],[-0.413324,0.04750787,0.9093437,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":0.7,"bbox":[-4,13,-2,13,-4,13],"windowRect":[100,128,772,608],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"/home/dyslexicon/R/x86_64-pc-linux-gnu-library/3.2/rgl/fonts/FreeSans.ttf","maxClipPlanes":8,"glVersion":3},"embeddings":{"viewport":"replace","projection":"replace","model":"replace"},"objects":[6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,5],"subscenes":[],"flags":19059}},"snapshot":"data:image/png;base64,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","width":673,"height":481,"sphereVerts":{"vb":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1],[0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0]],"it":[[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270],[17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288],[18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271]],"primitivetype":"triangle","material":null,"normals":null,"texcoords":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1]]},"context":{"shiny":false,"rmarkdown":"github_document"},"crosstalk":{"key":[],"group":[],"id":[],"options":[]}});
unnamed_chunk_3rgl.prefix = "unnamed_chunk_3";
</script>
<p id="unnamed_chunk_3debug">
You must enable Javascript to view this page properly.
</p>
<script>unnamed_chunk_3rgl.start();</script>
To produce the entire vineyard, or specify a range, run the same code but with the 'range' named argument changed to the range of time slices you prefer (up to how many you have). In this case there are 290.

If we want to investigate particular grapes in the vineyard we can do that by setting the named argument *id* to TRUE. With this option on we can right-click on a grape and see what time slice it is in and what index it has in the data allowing us to retrieve information about the feature.

``` r
vineyard(data,range=c(1,6),id=TRUE)
```

<script type="text/javascript">
var unnamed_chunk_4div = document.getElementById("unnamed_chunk_4div"),
unnamed_chunk_4rgl = new rglwidgetClass();
unnamed_chunk_4div.width = 1367;
unnamed_chunk_4div.height = 745;
unnamed_chunk_4rgl.initialize(unnamed_chunk_4div,
{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":true,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false},"rootSubscene":61,"objects":{"67":{"id":67,"type":"spheres","material":{"fog":false},"vertices":[[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[1.774331,4.141822,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[2.463811,5.256637,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.176749,4.601027,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.629513,4.248709,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.75255,4.690575,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.753949,4.372228,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[3.86918,4.48238,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.027844,4.613934,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.124667,6.443393,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.379174,4.989833,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.585269,5.395478,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[4.800633,5.510449,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[5.748493,6.50433,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1],[9.244714,10.34131,1]],"ignoreExtent":false,"flags":3},"68":{"id":68,"type":"spheres","material":{"fog":false},"vertices":[[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[6.149204,10.51344,2]],"ignoreExtent":false,"flags":3},"69":{"id":69,"type":"lines","material":{"alpha":[0.7647059,0.7647059,0.227451,0.227451,0.1372549,0.1372549,0.172549,0.172549,0.1960784,0.1960784,0.4666667,0.4666667,0.2705882,0.2705882,0.1607843,0.1607843,0.1372549,0.1372549,0.1686275,0.1686275,0.1215686,0.1215686,0.1215686,0.1215686,0.1490196,0.1490196,0.1490196,0.1490196,1,1,0.1803922,0.1803922,0.2627451,0.2627451,0.1372549,0.1372549,0.1647059,0.1647059,0.1098039,0.1098039,0.1019608,0.1019608,0.1372549,0.1372549],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.774331,4.141822,1],[1.883792,4.15316,2],[2.463811,5.256637,1],[2.452941,5.286639,2],[2.463811,5.256637,1],[3.743898,4.498632,2],[3.629513,4.248709,1],[2.452941,5.286639,2],[3.629513,4.248709,1],[3.743898,4.498632,2],[3.75255,4.690575,1],[3.744239,4.586135,2],[3.75255,4.690575,1],[3.757945,4.492146,2],[3.753949,4.372228,1],[3.743898,4.498632,2],[3.86918,4.48238,1],[2.452941,5.286639,2],[3.86918,4.48238,1],[3.743898,4.498632,2],[3.86918,4.48238,1],[4.127694,6.442847,2],[4.124667,6.443393,1],[3.743898,4.498632,2],[4.124667,6.443393,1],[4.127694,6.442847,2],[4.379174,4.989833,1],[4.58303,5.394935,2],[4.585269,5.395478,1],[4.58303,5.394935,2],[4.800633,5.510449,1],[2.452941,5.286639,2],[4.800633,5.510449,1],[3.438908,4.194337,2],[4.800633,5.510449,1],[3.743898,4.498632,2],[4.800633,5.510449,1],[4.127694,6.442847,2],[9.244714,10.34131,1],[2.452941,5.286639,2],[9.244714,10.34131,1],[4.127694,6.442847,2],[9.244714,10.34131,1],[6.149204,10.51344,2]],"colors":[[0,0,0,0.7647059],[0,0,0,0.7647059],[0,0,0,0.227451],[0,0,0,0.227451],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.172549],[0,0,0,0.172549],[0,0,0,0.1960784],[0,0,0,0.1960784],[0,0,0,0.4666667],[0,0,0,0.4666667],[0,0,0,0.2705882],[0,0,0,0.2705882],[0,0,0,0.1607843],[0,0,0,0.1607843],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.1686275],[0,0,0,0.1686275],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,1],[0,0,0,1],[0,0,0,0.1803922],[0,0,0,0.1803922],[0,0,0,0.2627451],[0,0,0,0.2627451],[0,0,0,0.1372549],[0,0,0,0.1372549],[0,0,0,0.1647059],[0,0,0,0.1647059],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.1019608],[0,0,0,0.1019608],[0,0,0,0.1372549],[0,0,0,0.1372549]],"centers":[[1.829062,4.147491,1.5],[2.458376,5.271638,1.5],[3.103855,4.877634,1.5],[3.041227,4.767674,1.5],[3.686706,4.373671,1.5],[3.748395,4.638355,1.5],[3.755248,4.591361,1.5],[3.748924,4.43543,1.5],[3.161061,4.88451,1.5],[3.806539,4.490506,1.5],[3.998437,5.462614,1.5],[3.934283,5.471012,1.5],[4.126181,6.44312,1.5],[4.481102,5.192384,1.5],[4.584149,5.395206,1.5],[3.626787,5.398544,1.5],[4.119771,4.852393,1.5],[4.272265,5.00454,1.5],[4.464163,5.976648,1.5],[5.848827,7.813972,1.5],[6.686204,8.392076,1.5],[7.696959,10.42737,1.5]],"ignoreExtent":false,"flags":16480},"70":{"id":70,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,1],[13,13,1]],"colors":[[0,0,0,1]],"centers":[[0,0,1],[13,13,1]],"ignoreExtent":false,"flags":64},"71":{"id":71,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,2],[13,13,2]],"colors":[[0,0,0,1]],"centers":[[0,0,2],[13,13,2]],"ignoreExtent":false,"flags":64},"72":{"id":72,"type":"spheres","material":{"fog":false},"vertices":[[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[1.883792,4.15316,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[2.452941,5.286639,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.438908,4.194337,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.743898,4.498632,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.744239,4.586135,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[3.757945,4.492146,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.127694,6.442847,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[4.58303,5.394935,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2],[6.149204,10.51344,2]],"ignoreExtent":false,"flags":3},"73":{"id":73,"type":"spheres","material":{"fog":false},"vertices":[[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[6.898338,10.81486,3]],"ignoreExtent":false,"flags":3},"74":{"id":74,"type":"lines","material":{"alpha":[0.7960784,0.7960784,0.3764706,0.3764706,0.2313726,0.2313726,0.2078431,0.2078431,0.1333333,0.1333333,0.5254902,0.5254902,0.1333333,0.1333333,0.1647059,0.1647059,1,1,0.1019608,0.1019608,0.3882353,0.3882353,0.6431373,0.6431373],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.883792,4.15316,2],[1.736609,4.125931,3],[2.452941,5.286639,2],[2.851068,5.54069,3],[3.743898,4.498632,2],[2.851068,5.54069,3],[3.743898,4.498632,2],[3.731448,4.250916,3],[3.743898,4.498632,2],[4.212198,6.406206,3],[3.744239,4.586135,2],[3.74173,4.685896,3],[3.744239,4.586135,2],[3.742332,4.486712,3],[3.757945,4.492146,2],[3.74173,4.685896,3],[3.757945,4.492146,2],[3.742332,4.486712,3],[4.127694,6.442847,2],[2.851068,5.54069,3],[4.127694,6.442847,2],[4.212198,6.406206,3],[4.58303,5.394935,2],[4.616407,5.433964,3]],"colors":[[0,0,0,0.7960784],[0,0,0,0.7960784],[0,0,0,0.3764706],[0,0,0,0.3764706],[0,0,0,0.2313726],[0,0,0,0.2313726],[0,0,0,0.2078431],[0,0,0,0.2078431],[0,0,0,0.1333333],[0,0,0,0.1333333],[0,0,0,0.5254902],[0,0,0,0.5254902],[0,0,0,0.1333333],[0,0,0,0.1333333],[0,0,0,0.1647059],[0,0,0,0.1647059],[0,0,0,1],[0,0,0,1],[0,0,0,0.1019608],[0,0,0,0.1019608],[0,0,0,0.3882353],[0,0,0,0.3882353],[0,0,0,0.6431373],[0,0,0,0.6431373]],"centers":[[1.8102,4.139545,2.5],[2.652005,5.413665,2.5],[3.297483,5.019661,2.5],[3.737673,4.374774,2.5],[3.978048,5.452419,2.5],[3.742984,4.636016,2.5],[3.743285,4.536423,2.5],[3.749837,4.589022,2.5],[3.750139,4.489429,2.5],[3.489381,5.991769,2.5],[4.169946,6.424526,2.5],[4.599719,5.41445,2.5]],"ignoreExtent":false,"flags":16480},"75":{"id":75,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,2],[13,13,2]],"colors":[[0,0,0,1]],"centers":[[0,0,2],[13,13,2]],"ignoreExtent":false,"flags":64},"76":{"id":76,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,3],[13,13,3]],"colors":[[0,0,0,1]],"centers":[[0,0,3],[13,13,3]],"ignoreExtent":false,"flags":64},"77":{"id":77,"type":"spheres","material":{"fog":false},"vertices":[[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[1.736609,4.125931,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[2.851068,5.54069,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.32144,4.004302,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.426822,4.022975,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.731448,4.250916,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.74173,4.685896,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[3.742332,4.486712,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.212198,6.406206,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.373624,5.07511,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[4.616407,5.433964,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3],[6.898338,10.81486,3]],"ignoreExtent":false,"flags":3},"78":{"id":78,"type":"spheres","material":{"fog":false},"vertices":[[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[8.20857,10.97004,4]],"ignoreExtent":false,"flags":3},"79":{"id":79,"type":"lines","material":{"alpha":[0.7647059,0.7647059,0.09803922,0.09803922,0.227451,0.227451,0.1215686,0.1215686,0.1764706,0.1764706,0.4901961,0.4901961,0.1098039,0.1098039,0.1098039,0.1098039,0.2196078,0.2196078,0.3490196,0.3490196,0.3686275,0.3686275,0.2431373,0.2431373,0.145098,0.145098,0.454902,0.454902,0.1098039,0.1098039,0.3176471,0.3176471,0.2156863,0.2156863,0.1098039,0.1098039,1,1,0.2901961,0.2901961],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.736609,4.125931,3],[1.732567,4.123106,4],[1.736609,4.125931,3],[4.118558,4.687138,4],[2.851068,5.54069,3],[4.062371,5.54427,4],[2.851068,5.54069,3],[4.13472,6.676558,4],[2.851068,5.54069,3],[4.187082,4.881758,4],[3.731448,4.250916,3],[3.76832,4.309572,4],[3.731448,4.250916,3],[4.13472,6.676558,4],[3.731448,4.250916,3],[4.187082,4.881758,4],[3.74173,4.685896,3],[3.756386,4.690742,4],[3.74173,4.685896,3],[4.118558,4.687138,4],[3.742332,4.486712,3],[3.756386,4.690742,4],[3.742332,4.486712,3],[4.118558,4.687138,4],[4.212198,6.406206,3],[3.76832,4.309572,4],[4.212198,6.406206,3],[4.13472,6.676558,4],[4.212198,6.406206,3],[4.187082,4.881758,4],[4.373624,5.07511,3],[3.76832,4.309572,4],[4.373624,5.07511,3],[4.13472,6.676558,4],[4.373624,5.07511,3],[4.187082,4.881758,4],[4.616407,5.433964,3],[4.687388,5.395462,4],[6.898338,10.81486,3],[8.20857,10.97004,4]],"colors":[[0,0,0,0.7647059],[0,0,0,0.7647059],[0,0,0,0.09803922],[0,0,0,0.09803922],[0,0,0,0.227451],[0,0,0,0.227451],[0,0,0,0.1215686],[0,0,0,0.1215686],[0,0,0,0.1764706],[0,0,0,0.1764706],[0,0,0,0.4901961],[0,0,0,0.4901961],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.2196078],[0,0,0,0.2196078],[0,0,0,0.3490196],[0,0,0,0.3490196],[0,0,0,0.3686275],[0,0,0,0.3686275],[0,0,0,0.2431373],[0,0,0,0.2431373],[0,0,0,0.145098],[0,0,0,0.145098],[0,0,0,0.454902],[0,0,0,0.454902],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.3176471],[0,0,0,0.3176471],[0,0,0,0.2156863],[0,0,0,0.2156863],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,1],[0,0,0,1],[0,0,0,0.2901961],[0,0,0,0.2901961]],"centers":[[1.734588,4.124518,3.5],[2.927584,4.406534,3.5],[3.45672,5.54248,3.5],[3.492894,6.108624,3.5],[3.519075,5.211224,3.5],[3.749884,4.280244,3.5],[3.933084,5.463737,3.5],[3.959265,4.566337,3.5],[3.749058,4.688319,3.5],[3.930144,4.686517,3.5],[3.749359,4.588727,3.5],[3.930445,4.586925,3.5],[3.990259,5.357889,3.5],[4.173459,6.541382,3.5],[4.19964,5.643982,3.5],[4.070972,4.692341,3.5],[4.254172,5.875834,3.5],[4.280353,4.978434,3.5],[4.651897,5.414713,3.5],[7.553453,10.89245,3.5]],"ignoreExtent":false,"flags":16480},"80":{"id":80,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,3],[13,13,3]],"colors":[[0,0,0,1]],"centers":[[0,0,3],[13,13,3]],"ignoreExtent":false,"flags":64},"81":{"id":81,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,4],[13,13,4]],"colors":[[0,0,0,1]],"centers":[[0,0,4],[13,13,4]],"ignoreExtent":false,"flags":64},"82":{"id":82,"type":"spheres","material":{"fog":false},"vertices":[[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[1.732567,4.123106,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.756386,4.690742,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[3.76832,4.309572,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.062371,5.54427,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.118558,4.687138,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.13472,6.676558,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.187082,4.881758,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[4.687388,5.395462,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4],[8.20857,10.97004,4]],"ignoreExtent":false,"flags":3},"83":{"id":83,"type":"spheres","material":{"fog":false},"vertices":[[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[7.315459,10.68141,5]],"ignoreExtent":false,"flags":3},"84":{"id":84,"type":"lines","material":{"alpha":[0.6509804,0.6509804,0.2784314,0.2784314,0.2588235,0.2588235,0.1686275,0.1686275,0.6078432,0.6078432,0.1098039,0.1098039,0.2352941,0.2352941,0.3254902,0.3254902,0.4352941,0.4352941,0.2431373,0.2431373,0.1490196,0.1490196,0.09803922,0.09803922,0.4823529,0.4823529,0.1058824,0.1058824,0.1294118,0.1294118,0.254902,0.254902,1,1,0.2862745,0.2862745],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.732567,4.123106,4],[1.741514,4.13105,5],[3.756386,4.690742,4],[3.792702,4.679107,5],[3.756386,4.690742,4],[3.793629,4.583196,5],[3.76832,4.309572,4],[3.942643,5.100069,5],[3.76832,4.309572,4],[4.106479,4.662616,5],[3.76832,4.309572,4],[4.234904,6.410672,5],[3.76832,4.309572,4],[4.259863,4.783735,5],[4.062371,5.54427,4],[3.436065,5.516401,5],[4.118558,4.687138,4],[3.792702,4.679107,5],[4.118558,4.687138,4],[3.793629,4.583196,5],[4.13472,6.676558,4],[3.942643,5.100069,5],[4.13472,6.676558,4],[4.106479,4.662616,5],[4.13472,6.676558,4],[4.234904,6.410672,5],[4.13472,6.676558,4],[4.259863,4.783735,5],[4.187082,4.881758,4],[3.436065,5.516401,5],[4.187082,4.881758,4],[3.942643,5.100069,5],[4.687388,5.395462,4],[4.659994,5.385197,5],[8.20857,10.97004,4],[7.315459,10.68141,5]],"colors":[[0,0,0,0.6509804],[0,0,0,0.6509804],[0,0,0,0.2784314],[0,0,0,0.2784314],[0,0,0,0.2588235],[0,0,0,0.2588235],[0,0,0,0.1686275],[0,0,0,0.1686275],[0,0,0,0.6078432],[0,0,0,0.6078432],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.2352941],[0,0,0,0.2352941],[0,0,0,0.3254902],[0,0,0,0.3254902],[0,0,0,0.4352941],[0,0,0,0.4352941],[0,0,0,0.2431373],[0,0,0,0.2431373],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.09803922],[0,0,0,0.09803922],[0,0,0,0.4823529],[0,0,0,0.4823529],[0,0,0,0.1058824],[0,0,0,0.1058824],[0,0,0,0.1294118],[0,0,0,0.1294118],[0,0,0,0.254902],[0,0,0,0.254902],[0,0,0,1],[0,0,0,1],[0,0,0,0.2862745],[0,0,0,0.2862745]],"centers":[[1.73704,4.127078,4.5],[3.774544,4.684925,4.5],[3.775007,4.636969,4.5],[3.855481,4.704821,4.5],[3.9374,4.486094,4.5],[4.001612,5.360122,4.5],[4.014091,4.546653,4.5],[3.749218,5.530335,4.5],[3.95563,4.683123,4.5],[3.956094,4.635167,4.5],[4.038682,5.888313,4.5],[4.1206,5.669587,4.5],[4.184813,6.543615,4.5],[4.197291,5.730146,4.5],[3.811574,5.19908,4.5],[4.064862,4.990913,4.5],[4.673691,5.390329,4.5],[7.762014,10.82572,4.5]],"ignoreExtent":false,"flags":16480},"85":{"id":85,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,4],[13,13,4]],"colors":[[0,0,0,1]],"centers":[[0,0,4],[13,13,4]],"ignoreExtent":false,"flags":64},"86":{"id":86,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,5],[13,13,5]],"colors":[[0,0,0,1]],"centers":[[0,0,5],[13,13,5]],"ignoreExtent":false,"flags":64},"87":{"id":87,"type":"spheres","material":{"fog":false},"vertices":[[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[1.741514,4.13105,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.436065,5.516401,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.792702,4.679107,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.793629,4.583196,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[3.942643,5.100069,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.106479,4.662616,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.234904,6.410672,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.259863,4.783735,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[4.659994,5.385197,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5],[7.315459,10.68141,5]],"ignoreExtent":false,"flags":3},"88":{"id":88,"type":"spheres","material":{"fog":false},"vertices":[[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6]],"colors":[[1,0,0,1]],"radii":[[0.1]],"centers":[[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6],[1.848939,4.123323,6],[3.215719,4.137128,6],[3.504768,4.017168,6],[3.741845,4.624436,6],[3.786937,5.357449,6],[3.858304,4.5126,6],[3.891402,4.609963,6],[3.940814,4.612162,6],[4.151063,6.782679,6],[4.196352,4.797724,6],[4.582591,5.389624,6],[4.692433,5.343936,6],[6.991272,9.86127,6]],"ignoreExtent":false,"flags":3},"89":{"id":89,"type":"lines","material":{"alpha":[1,1,0.1058824,0.1058824,0.8627451,0.8627451,0.1137255,0.1137255,0.1294118,0.1294118,0.7803922,0.7803922,0.6980392,0.6980392,0.1490196,0.1490196,0.4509804,0.4509804,0.2705882,0.2705882,0.282353,0.282353,0.9882353,0.9882353,0.2078431,0.2078431,0.172549,0.172549,0.2313726,0.2313726,0.3333333,0.3333333,0.4862745,0.4862745,0.1686275,0.1686275,0.1098039,0.1098039,0.7921569,0.7921569,0.5215687,0.5215687],"lit":false,"lwd":3,"fog":false,"isTransparent":true},"vertices":[[1.741514,4.13105,5],[1.848939,4.123323,6],[1.741514,4.13105,5],[3.891402,4.609963,6],[3.436065,5.516401,5],[3.786937,5.357449,6],[3.436065,5.516401,5],[4.151063,6.782679,6],[3.792702,4.679107,5],[1.848939,4.123323,6],[3.792702,4.679107,5],[3.741845,4.624436,6],[3.792702,4.679107,5],[3.891402,4.609963,6],[3.793629,4.583196,5],[3.741845,4.624436,6],[3.793629,4.583196,5],[3.891402,4.609963,6],[3.942643,5.100069,5],[3.786937,5.357449,6],[3.942643,5.100069,5],[3.858304,4.5126,6],[4.106479,4.662616,5],[3.858304,4.5126,6],[4.106479,4.662616,5],[4.151063,6.782679,6],[4.234904,6.410672,5],[3.786937,5.357449,6],[4.234904,6.410672,5],[3.858304,4.5126,6],[4.234904,6.410672,5],[4.151063,6.782679,6],[4.259863,4.783735,5],[3.858304,4.5126,6],[4.259863,4.783735,5],[4.151063,6.782679,6],[4.659994,5.385197,5],[4.196352,4.797724,6],[4.659994,5.385197,5],[4.582591,5.389624,6],[7.315459,10.68141,5],[6.991272,9.86127,6]],"colors":[[0,0,0,1],[0,0,0,1],[0,0,0,0.1058824],[0,0,0,0.1058824],[0,0,0,0.8627451],[0,0,0,0.8627451],[0,0,0,0.1137255],[0,0,0,0.1137255],[0,0,0,0.1294118],[0,0,0,0.1294118],[0,0,0,0.7803922],[0,0,0,0.7803922],[0,0,0,0.6980392],[0,0,0,0.6980392],[0,0,0,0.1490196],[0,0,0,0.1490196],[0,0,0,0.4509804],[0,0,0,0.4509804],[0,0,0,0.2705882],[0,0,0,0.2705882],[0,0,0,0.282353],[0,0,0,0.282353],[0,0,0,0.9882353],[0,0,0,0.9882353],[0,0,0,0.2078431],[0,0,0,0.2078431],[0,0,0,0.172549],[0,0,0,0.172549],[0,0,0,0.2313726],[0,0,0,0.2313726],[0,0,0,0.3333333],[0,0,0,0.3333333],[0,0,0,0.4862745],[0,0,0,0.4862745],[0,0,0,0.1686275],[0,0,0,0.1686275],[0,0,0,0.1098039],[0,0,0,0.1098039],[0,0,0,0.7921569],[0,0,0,0.7921569],[0,0,0,0.5215687],[0,0,0,0.5215687]],"centers":[[1.795226,4.127186,5.5],[2.816458,4.370506,5.5],[3.611501,5.436925,5.5],[3.793564,6.14954,5.5],[2.820821,4.401215,5.5],[3.767274,4.651772,5.5],[3.842052,4.644535,5.5],[3.767737,4.603816,5.5],[3.842516,4.59658,5.5],[3.86479,5.228759,5.5],[3.900473,4.806334,5.5],[3.982391,4.587608,5.5],[4.128771,5.722648,5.5],[4.010921,5.88406,5.5],[4.046604,5.461636,5.5],[4.192984,6.596675,5.5],[4.059083,4.648168,5.5],[4.205463,5.783207,5.5],[4.428173,5.09146,5.5],[4.621292,5.38741,5.5],[7.153366,10.27134,5.5]],"ignoreExtent":false,"flags":16480},"90":{"id":90,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,5],[13,13,5]],"colors":[[0,0,0,1]],"centers":[[0,0,5],[13,13,5]],"ignoreExtent":false,"flags":64},"91":{"id":91,"type":"linestrip","material":{"lit":false,"fog":false},"vertices":[[0,0,6],[13,13,6]],"colors":[[0,0,0,1]],"centers":[[0,0,6],[13,13,6]],"ignoreExtent":false,"flags":64},"92":{"id":92,"type":"lines","material":{"lit":false,"fog":false},"vertices":[[0,-0.195,6.178],[12,-0.195,6.178],[0,-0.195,6.178],[0,-0.52975,6.3119],[2,-0.195,6.178],[2,-0.52975,6.3119],[4,-0.195,6.178],[4,-0.52975,6.3119],[6,-0.195,6.178],[6,-0.52975,6.3119],[8,-0.195,6.178],[8,-0.52975,6.3119],[10,-0.195,6.178],[10,-0.52975,6.3119],[12,-0.195,6.178],[12,-0.52975,6.3119]],"colors":[[0,0,0,1]],"centers":[[6,-0.195,6.178],[0,-0.362375,6.24495],[2,-0.362375,6.24495],[4,-0.362375,6.24495],[6,-0.362375,6.24495],[8,-0.362375,6.24495],[10,-0.362375,6.24495],[12,-0.362375,6.24495]],"ignoreExtent":true,"flags":64},"93":{"id":93,"type":"text","material":{"lit":false,"fog":false},"vertices":[[0,-1.19925,6.5797],[2,-1.19925,6.5797],[4,-1.19925,6.5797],[6,-1.19925,6.5797],[8,-1.19925,6.5797],[10,-1.19925,6.5797],[12,-1.19925,6.5797]],"colors":[[0,0,0,1]],"texts":[[" 0"],[" 2"],[" 4"],[" 6"],[" 8"],["10"],["12"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[0,-1.19925,6.5797],[2,-1.19925,6.5797],[4,-1.19925,6.5797],[6,-1.19925,6.5797],[8,-1.19925,6.5797],[10,-1.19925,6.5797],[12,-1.19925,6.5797]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"94":{"id":94,"type":"lines","material":{"lit":false,"fog":false},"vertices":[[-0.195,0,0.822],[-0.195,12,0.822],[-0.195,0,0.822],[-0.52975,0,0.6881],[-0.195,2,0.822],[-0.52975,2,0.6881],[-0.195,4,0.822],[-0.52975,4,0.6881],[-0.195,6,0.822],[-0.52975,6,0.6881],[-0.195,8,0.822],[-0.52975,8,0.6881],[-0.195,10,0.822],[-0.52975,10,0.6881],[-0.195,12,0.822],[-0.52975,12,0.6881]],"colors":[[0,0,0,1]],"centers":[[-0.195,6,0.822],[-0.362375,0,0.7550499],[-0.362375,2,0.7550499],[-0.362375,4,0.7550499],[-0.362375,6,0.7550499],[-0.362375,8,0.7550499],[-0.362375,10,0.7550499],[-0.362375,12,0.7550499]],"ignoreExtent":true,"flags":64},"95":{"id":95,"type":"text","material":{"lit":false,"fog":false},"vertices":[[-1.19925,0,0.4203],[-1.19925,2,0.4203],[-1.19925,4,0.4203],[-1.19925,6,0.4203],[-1.19925,8,0.4203],[-1.19925,10,0.4203],[-1.19925,12,0.4203]],"colors":[[0,0,0,1]],"texts":[[" 0"],[" 2"],[" 4"],[" 6"],[" 8"],["10"],["12"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-1.19925,0,0.4203],[-1.19925,2,0.4203],[-1.19925,4,0.4203],[-1.19925,6,0.4203],[-1.19925,8,0.4203],[-1.19925,10,0.4203],[-1.19925,12,0.4203]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"96":{"id":96,"type":"lines","material":{"lit":false,"fog":false},"vertices":[[-0.195,-0.195,1],[-0.195,-0.195,6],[-0.195,-0.195,1],[-0.52975,-0.52975,1],[-0.195,-0.195,2],[-0.52975,-0.52975,2],[-0.195,-0.195,3],[-0.52975,-0.52975,3],[-0.195,-0.195,4],[-0.52975,-0.52975,4],[-0.195,-0.195,5],[-0.52975,-0.52975,5],[-0.195,-0.195,6],[-0.52975,-0.52975,6]],"colors":[[0,0,0,1]],"centers":[[-0.195,-0.195,3.5],[-0.362375,-0.362375,1],[-0.362375,-0.362375,2],[-0.362375,-0.362375,3],[-0.362375,-0.362375,4],[-0.362375,-0.362375,5],[-0.362375,-0.362375,6]],"ignoreExtent":true,"flags":64},"97":{"id":97,"type":"text","material":{"lit":false,"fog":false},"vertices":[[-1.19925,-1.19925,1],[-1.19925,-1.19925,2],[-1.19925,-1.19925,3],[-1.19925,-1.19925,4],[-1.19925,-1.19925,5],[-1.19925,-1.19925,6]],"colors":[[0,0,0,1]],"texts":[["1"],["2"],["3"],["4"],["5"],["6"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-1.19925,-1.19925,1],[-1.19925,-1.19925,2],[-1.19925,-1.19925,3],[-1.19925,-1.19925,4],[-1.19925,-1.19925,5],[-1.19925,-1.19925,6]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"98":{"id":98,"type":"text","material":{"lit":false},"vertices":[[-2,-2,3]],"colors":[[0,0,0,1]],"texts":[["Time"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-2,-2,3]],"family":[["sans"]],"font":[[1]],"ignoreExtent":false,"flags":2064},"99":{"id":99,"type":"text","material":{"lit":false},"vertices":[[-4,6,-4]],"colors":[[0,0,0,1]],"texts":[["Death"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-4,6,-4]],"family":[["sans"]],"font":[[1]],"ignoreExtent":false,"flags":2064},"100":{"id":100,"type":"text","material":{"lit":false},"vertices":[[6,-2,9]],"colors":[[0,0,0,1]],"texts":[["Birth"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[6,-2,9]],"family":[["sans"]],"font":[[1]],"ignoreExtent":false,"flags":2064},"101":{"id":101,"type":"text","material":{"lit":false},"vertices":[[1.774331,4.141822,1]],"colors":[[0,0,0,1]],"texts":[["H1 at 1 : 4775"]],"cex":[[1]],"adj":[[-0.1,0.5]],"centers":[[1.774331,4.141822,1]],"family":[["sans"]],"font":[[1]],"ignoreExtent":false,"flags":2064},"65":{"id":65,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"64":{"id":64,"type":"background","material":{},"colors":[[0.2980392,0.2980392,0.2980392,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"66":{"id":66,"type":"background","material":{"lit":false,"back":"lines","fog":false},"colors":[[1,1,1,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"61":{"id":61,"type":"subscene","par3d":{"antialias":0,"FOV":30,"ignoreExtent":false,"listeners":61,"mouseMode":{"left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,50.48753],"modelMatrix":[[0.9105204,0.009738045,0.41335,-5.184276],[0.01078209,0.9988235,-0.0472818,-5.423844],[-0.413324,0.04750787,0.9093437,-51.16222],[0,0,0,1]],"projMatrix":[[2.903834,0,0,0],[0,5.331501,0,0],[0,0,-3.863703,-182.0017],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[0.9105204,0.009738045,0.41335,0],[0.01078209,0.9988235,-0.0472818,0],[-0.413324,0.04750787,0.9093437,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":0.7,"bbox":[-4,13,-2,13,-4,9],"windowRect":[0,24,1366,768],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"/home/dyslexicon/R/x86_64-pc-linux-gnu-library/3.2/rgl/fonts/FreeSans.ttf","maxClipPlanes":8,"glVersion":3},"embeddings":{"viewport":"replace","projection":"replace","model":"replace"},"objects":[66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,65],"subscenes":[],"flags":19059}},"snapshot":"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABVYAAALoCAIAAAAcPDnOAAAAHXRFWHRTb2Z0d2FyZQBSL1JHTCBwYWNrYWdlL2xpYnBuZ7GveO8AACAASURBVHic7N0HmFTV/f/xz52dZSmK2ECFiIoSARGNvcbYImJsUTGShESjQQk2VCz5aVTURIxEjRqDYgdCEKWpiAVBkY6IjaZEAbGASpU6//M/95l5LtN2dqeemffr4dlndna+M3fZO+eez3duUQQAAAAAAFQAFXsBAAAAAABAIdACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItACAAAAAACgItAC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Tz01ePDgHj16/M3f/E1NTc3JJ5+8du3apDsFwAEpAQAk6Re/+EXfvn0POuigww8/PJfLnXrqqevWrdvvK5cuXdqnT5+FCxeedtppSgBkW8YmvHjwwQej5H/IIYfccsstixYtuvXWW2tray+++OKk+1UyO3bseOyxx2bOnPm73/0u6b4A0AYlAICEbd68ee7cuffee++yZcsO9Jpt27bV1dVNmzYtWlYCINuyN+FFYYa8G2+8sbgmWu7evXsrs34AQJkoAQCkwKRJk04//fTCvcSVAMiwTE54sWzZsmiLnnnmmeKaxx9/PFrz/PPPJ9grAKqTEgBApZs9e3b//v3/9Kc/FR4qAZBhmZzwYufOnb179/7JT35SXDN9+vRcLrdx48YEewVAdVICAKh0U6dOjdJCt31CCIWHc+bMafniKFTUv92SJUvi7zN0QoYnvJg0adKgQYOWL1++N3830KOOOuqcc85JulMAVCMlAIBKt2rVqkeaGDly5Jlnnhkt7PcQ4t133x3e7rOf/Wz8fYaOyvaEF5s3bz7ppJNyudzgwYNramrOOOOM1157LelOAVCNlAAAUqb1dHT99dePGDFiaxO7du2Ks3uU3J49exYvXjx79uwFCxbs3r076e6US+YnvIh+j/Pnz6+vrzdtPgAJUgIASJnW09GnP/3p8847L87+UFYvvvjiiBEjQgj9+/fP5XJ1dXWrVq1KulOlZ8ILAIiHEgBAppx66qlf/vKX582b973vfe/ee+/dunVr0j2iS8aMGTNs2LAVK1ZEy6tXr46WR48enXSnSq9DE14AAJ2mBACQKYMGDerTp8+hhx56/PHH9+jRY8iQIYX0SBpt3rw5CsMzZsworrn99tujNS+//HKCvSqHDk14AQB0mhIAQHbs3Llz1KhRV1555VtvvRU9XLt27dFHH33qqacm3S86ad26dZdeeunq1auLa+66664QwoYNGxLsVQxcCAAAZaIEAJBlhYPGmU+MVWLTpk2jRo2K4nHSHSk7JQAAKBMlAIAse+KJJ0IIzz//fCuvWbt27ezZs5955pnCZOypsGTJkvvvv3/lypVJdyQ+9fX1Q4YMGTZsWPT7SrovAEBaKQEAZEcU+E855ZT169cX19x2223du3d/4403DtRkypQpNTU1Bx98cPT/4447rvLvVb5t27axY8fmcrm+ffuGECZPntzY2Jh0p8przZo1Y8aM6dWrV/TLMr8jANAVSgAA2bFp06ZDDjnkwx/+8JYtW6KHy5cvP+KIIz71qU8d6PW33357jx49fv3rX+/NzzY/ZMiQiy66KLbeds6VV14Zhf+lS5fuzR8YDyHcc889SXeqjBYtWtSvX79zzz3XwX8AoOuUAAAy5YEHHujfv3/Pnj0HDx6cy+XOP//8Vo4bDx8+fPLkycWH991331e+8pVYutlJDQ0Nhx122FVXXVVcc1Zegl0qq8bGxuh3NHHixMyf6QAAxEMJACBrtmzZ8tBDD82aNWv58uWtvGzdunUhhMIpADt37kxFyFy1alXU57lz5xbXTJs2rW/fvgl2qawWLVoUbe/VV189/e127NiRdNcAgFRSAgCoUk8//XQUL3/6058ed9xx0UJtbe3kyZN37tyZdL9aM2/evKirK1asKK654447ojWtTHaQaj/72c/C/jSd7gEAoP2UAACq1IMPPhiFyUMOOeSWW25ZtGjRrbfeWltbe/HFFyfdr9Y88MADUZ9feuml4pqZM2dGa9atW5dgrwAA0kIJAKBKPfzww1F4vvHGG4trouXu3bsXphKsTI899ljU56b3ApwxY0a0Ztu2bQn2CgAgLZQAAKrUsmXLovD8zDPPFNc8/vjj0Zrnn38+wV61bsWKFVEPo34W19xwww21tbUJdgkAIEWUAACq1M6dO3v37v2Tn/ykuGb69Om5XG7jxo0J9qp1e/bsGThw4PXXX19cM378+HHjxiXYJQCAFFECAKhekyZNGjRoUOHGAWvWrDnqqKPOOeecpDvVhqlTpw4ePPiVV16Jlh999NFcLjdr1qykOwUAkA5KAADVa/PmzSeddFKUoqNQXVNTc8YZZ7z22mtJd6oN27dvHzNmTJ8+ferq6qI+X3755Un3CAAgNZQAgGr0/e9//4IDmD179qRJk+bNm5d0H2OyZ8+e+fPn19fX/+53v0u6L+3V2Ni4cOHCqM/PPfdc0n0BAEgTJQCgGv3oRz+auE+/fv2GDh1afPirX/0qWvPjH/846T4CAECJKQEA1W7kyJEXXHBB0zUNDQ2NjY1J9QcAAMpECQCodi1LAHfeeWfxzvMzZ85cs2bNb37zmy984QtTp05dsWJFY2PjrFmzLr300muuueaPf/xjsdXGjRu/853vTJ48+Vvf+tarr74a5yYAAEB7KAEA1a5lCWDAgAHFCwGGDh161llnjRgxIsr2RxxxRL9+/T760Y++973vveSSS2pra6M1b731VvSyhQsX9u3bN3qrT37yk0ceeWS0nKJL6wEAqBJKAEC1a7MEMGrUqO3bt0fLf/jDH0II48aNa2hoiB7W19dHD6OVjY2NdXV15513XmH9zp07Tz755Pe85z2xbwoAALRGCQCodm2WAK699trC8u7du6PM//Of/7zwsFARePbZZ5ctWxYtND3sP3PmzGjNpk2bYtkCAABoFyUAoNq1WQL4p3/6p8JyoQTwi1/8ovBw5cqVhRLAfffdFy0ce+yxI/c56qijCicIxLkhAADQOiUAoNp1vQTw4IMPRgtz5sz5zdtt3bo1zg0BAIDWKQEA1a7rJYDVq1dHC0888UTxHZ555pmvf/3rsXQfAADaSwkAqHZdLwFEy2PGjBk9enThXoB//OMfjz766I9//OPxbQMAALSDEgBQ7UpSAohi//Dhw7t37/63f/u33bp1e+9737thw4YYNwIAANqmBABQGg0NDU899dTdd9+9YMGCxsbGpLsDAADNKQEAAABAVVACAAAAgKqgBAAAAABVQQkAAAAAqoISAAAAAFQFJQAAAACoCkoAAAAAUBWUAAAAAKAqKAEAAABAVVACAAAAgKqgBAAAkB033HDDBU187nOfmzFjxu7duwvPTpo0ad68ee15n8suu+zBBx8sZ08BSIASAABAdowdO3bQoEET8y688MLTTz89l8udccYZDQ0N0bP9+vX78Y9/vN+Gt9xyy7e//e3iw6FDh958880xdRqAuCgBAABkx9ixY6PA33TNv/7rv4YQfv3rX0fLDQ0NjY2N+204ceLE8847r/hQCQAgk5QAAACyo2UJ4M9//nMI4dZbb42W77zzzpUrVxbW33PPPS+++OLChQsnT5782muvnXLKKSeccML06dPffPPNvftKAL///e+nTp36uc99btasWbFvCgClpwQAAJAdLUsAd911Vy6Xe/bZZ6PlAQMGFC8EGDJkyBVXXHHQQQfV1dW98sor0cPBgwdPmDBh27Zte/MlgLPPPvuYY46J8v+ZZ54ZQvje974X+9YAUGJKAAAA2dF0LoBPfvKT73//+3v06HHbbbcVnm1WAujbt+/SpUsLD1teCDB48ODXX3+98PDss8+O3irG7QCgLJQAAACyY+zYsYcddthH95kwYcLAgQPf/e53r127dm+LEsDFF19cbNiyBPDFL36x+HDq1Kknn3xyXBsBQLkoAQAAZEfLCwG2bt167LHHjhs3bm+LEsCNN95YfFnLEsBNN91UfKgEAJANSgAAANnRsgQQmTJlSr9+/fa2KAE0nfO/9TsCKAEAZIMSAABAduy3BDB+/PhRo0btVQIAqHpKAAAA2TF27NiRI0f+cp+ZM2d+9rOfDSH88z//8962SgDnnHNO8aESAEAmKQEAAGTH2LFjQxPdu3cfPnz4rbfe2tjYuLfVEsDVV19dU1MzatSoLVu27FUCAMgoJQAAAPa++eabc+bMmT179u7du5PuCwDlogQAAAAAVUEJAAAAAKqCEgAAAABUBSUAAAAAqApKAAAAAFAVlAAAAACgKigBAAAAQFVQAgAAAICqoAQAAAAAVUEJAAAAAKqCEgAAAABUBSUAAAAAqApKAAAAAFAVlAAAAACgKigBAAAAQFVQAgAAAICqoAQAAAAAVUEJAAAAAKqCEgAAAABUBSUAAAAAqApKAAAAAFAVlAAAAACgKigBAAAAQFVQAgAAAICqoAQAAAAAVUEJAAAAAKrC/wGEz3m+vz0nugAAAABJRU5ErkJggg==","width":1367,"height":745,"sphereVerts":{"reuse":"unnamed_chunk_3div"},"context":{"shiny":false,"rmarkdown":"github_document"},"crosstalk":{"key":[],"group":[],"id":[],"options":[]}});
unnamed_chunk_4rgl.prefix = "unnamed_chunk_4";
</script>
<p id="unnamed_chunk_4debug">
You must enable Javascript to view this page properly.
</p>
<script>unnamed_chunk_4rgl.start();</script>
Now that we know the index of the grape in question we can plot the loop it represents in the acc.

``` r
plot_loops(loops = list(data[[1]]$cycleLocation[[4775]]),data=acc)
```

<script type="text/javascript">
var unnamed_chunk_5div = document.getElementById("unnamed_chunk_5div"),
unnamed_chunk_5rgl = new rglwidgetClass();
unnamed_chunk_5div.width = 673;
unnamed_chunk_5div.height = 481;
unnamed_chunk_5rgl.initialize(unnamed_chunk_5div,
{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":false,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false},"rootSubscene":102,"objects":{"108":{"id":108,"type":"points","material":{"lit":false},"vertices":[[30,72,41],[30,73,38],[30,73,39],[31,58,53],[31,58,54],[31,58,55],[31,58,56],[31,59,53],[31,59,54],[31,59,55],[31,59,56],[31,60,53],[31,60,54],[31,60,55],[31,60,56],[31,70,41],[31,70,42],[31,70,43],[31,70,44],[31,71,39],[31,71,40],[31,71,41],[31,71,42],[31,71,43],[31,71,44],[31,72,39],[31,72,40],[31,72,41],[31,72,42],[31,72,43],[31,72,44],[31,72,45],[31,73,37],[31,73,38],[31,73,39],[31,73,40],[31,73,41],[31,73,42],[31,74,38],[31,74,39],[31,74,40],[31,75,36],[31,75,37],[31,75,38],[31,76,36],[31,76,37],[31,76,38],[31,77,36],[31,77,37],[31,77,38],[32,57,50],[32,57,51],[32,57,52],[32,57,53],[32,58,51],[32,58,53],[32,58,54],[32,58,55],[32,58,56],[32,59,53],[32,59,54],[32,59,55],[32,59,56],[32,60,48],[32,60,49],[32,60,50],[32,60,53],[32,60,54],[32,60,55],[32,60,56],[32,61,48],[32,61,49],[32,61,50],[32,62,48],[32,62,49],[32,62,50],[32,65,48],[32,65,49],[32,65,50],[32,66,47],[32,66,48],[32,66,49],[32,66,50],[32,66,51],[32,67,47],[32,67,48],[32,67,49],[32,67,50],[32,67,51],[32,70,41],[32,70,42],[32,70,43],[32,70,44],[32,71,39],[32,71,40],[32,71,41],[32,71,42],[32,71,43],[32,71,44],[32,72,38],[32,72,39],[32,72,40],[32,72,41],[32,72,42],[32,72,43],[32,72,44],[32,72,45],[32,73,37],[32,73,38],[32,73,39],[32,73,40],[32,73,41],[32,74,37],[32,74,38],[32,74,39],[32,74,40],[32,74,41],[32,75,29],[32,75,30],[32,75,31],[32,75,32],[32,75,36],[32,76,29],[32,76,30],[32,76,31],[32,76,34],[32,76,35],[32,76,36],[32,77,30],[32,77,31],[33,57,50],[33,57,51],[33,57,52],[33,57,53],[33,58,50],[33,58,51],[33,58,52],[33,58,53],[33,59,48],[33,59,49],[33,59,50],[33,59,51],[33,59,52],[33,59,53],[33,59,54],[33,59,55],[33,59,56],[33,60,48],[33,60,49],[33,60,50],[33,60,53],[33,60,54],[33,60,55],[33,60,56],[33,61,48],[33,61,49],[33,61,50],[33,61,53],[33,61,54],[33,61,55],[33,61,56],[33,62,47],[33,62,48],[33,62,49],[33,62,50],[33,63,47],[33,63,48],[33,64,47],[33,64,48],[33,65,48],[33,65,49],[33,65,50],[33,66,47],[33,66,48],[33,66,49],[33,66,50],[33,66,51],[33,67,45],[33,67,46],[33,67,47],[33,67,48],[33,67,49],[33,67,50],[33,67,51],[33,68,45],[33,68,46],[33,68,47],[33,69,45],[33,69,46],[33,69,47],[33,70,41],[33,70,42],[33,70,43],[33,70,44],[33,70,45],[33,70,46],[33,71,39],[33,71,40],[33,71,41],[33,71,42],[33,71,43],[33,71,44],[33,72,37],[33,72,38],[33,72,39],[33,72,40],[33,72,41],[33,72,42],[33,72,43],[33,72,44],[33,72,45],[33,73,37],[33,73,38],[33,73,39],[33,73,40],[33,74,34],[33,74,35],[33,74,36],[33,74,37],[33,74,38],[33,74,39],[33,74,40],[33,75,29],[33,75,30],[33,75,31],[33,75,32],[33,75,34],[33,75,35],[33,75,36],[33,75,37],[33,75,38],[33,76,29],[33,76,30],[33,76,31],[33,76,32],[33,76,34],[33,76,35],[33,76,36],[33,76,37],[33,76,38],[33,77,29],[33,77,30],[33,77,31],[33,77,32],[33,78,29],[33,78,30],[33,78,31],[33,78,32],[34,57,50],[34,57,51],[34,57,52],[34,57,53],[34,58,50],[34,58,51],[34,58,52],[34,58,53],[34,59,48],[34,59,49],[34,59,50],[34,59,51],[34,59,52],[34,59,53],[34,59,54],[34,59,55],[34,59,56],[34,60,48],[34,60,49],[34,60,50],[34,60,53],[34,60,54],[34,60,55],[34,60,56],[34,61,48],[34,61,49],[34,61,50],[34,61,53],[34,61,54],[34,61,55],[34,61,56],[34,62,47],[34,62,48],[34,62,49],[34,62,50],[34,63,47],[34,63,48],[34,64,47],[34,64,48],[34,65,46],[34,65,47],[34,65,48],[34,65,49],[34,65,50],[34,65,51],[34,65,52],[34,65,53],[34,66,46],[34,66,47],[34,66,48],[34,66,49],[34,66,50],[34,66,51],[34,66,52],[34,67,45],[34,67,46],[34,67,47],[34,67,48],[34,67,49],[34,67,50],[34,67,51],[34,68,45],[34,68,46],[34,69,45],[34,69,46],[34,70,41],[34,70,42],[34,70,43],[34,70,44],[34,70,45],[34,70,46],[34,71,39],[34,71,40],[34,71,41],[34,71,42],[34,71,43],[34,71,44],[34,71,45],[34,72,37],[34,72,38],[34,72,39],[34,72,40],[34,72,41],[34,72,42],[34,72,43],[34,72,44],[34,72,45],[34,73,37],[34,73,38],[34,73,39],[34,73,40],[34,74,34],[34,74,35],[34,74,36],[34,74,37],[34,74,40],[34,75,29],[34,75,32],[34,75,34],[34,75,35],[34,75,36],[34,75,37],[34,75,38],[34,76,29],[34,76,30],[34,76,31],[34,76,32],[34,76,34],[34,76,35],[34,76,36],[34,76,37],[34,76,38],[34,77,29],[34,77,30],[34,77,31],[34,77,32],[34,78,29],[34,78,30],[34,78,31],[34,78,32],[34,79,30],[34,79,31],[35,57,50],[35,57,51],[35,57,52],[35,57,53],[35,58,50],[35,58,51],[35,58,52],[35,58,53],[35,59,48],[35,59,49],[35,59,50],[35,59,51],[35,59,52],[35,59,53],[35,59,54],[35,59,55],[35,59,56],[35,60,48],[35,60,49],[35,60,50],[35,60,53],[35,60,54],[35,60,55],[35,60,56],[35,61,48],[35,61,49],[35,61,50],[35,61,53],[35,61,54],[35,61,55],[35,61,56],[35,62,46],[35,62,47],[35,62,48],[35,62,49],[35,62,50],[35,63,45],[35,63,46],[35,63,47],[35,63,48],[35,63,50],[35,63,51],[35,63,52],[35,63,53],[35,64,45],[35,64,46],[35,64,47],[35,64,48],[35,64,50],[35,64,51],[35,64,52],[35,64,53],[35,65,43],[35,65,44],[35,65,45],[35,65,46],[35,65,47],[35,65,48],[35,65,49],[35,65,50],[35,65,51],[35,65,52],[35,65,53],[35,66,43],[35,66,44],[35,66,47],[35,66,48],[35,66,49],[35,66,50],[35,66,51],[35,66,52],[35,67,43],[35,67,44],[35,67,45],[35,67,46],[35,67,47],[35,67,48],[35,68,43],[35,68,44],[35,68,45],[35,68,46],[35,69,45],[35,69,46],[35,69,47],[35,70,27],[35,70,28],[35,70,29],[35,70,30],[35,70,41],[35,70,45],[35,70,46],[35,70,47],[35,71,27],[35,71,28],[35,71,29],[35,71,30],[35,71,38],[35,71,39],[35,71,40],[35,71,41],[35,71,42],[35,71,43],[35,71,44],[35,71,45],[35,71,46],[35,72,27],[35,72,28],[35,72,29],[35,72,30],[35,72,37],[35,72,38],[35,72,39],[35,72,40],[35,72,41],[35,72,42],[35,72,43],[35,73,37],[35,73,38],[35,73,39],[35,73,40],[35,73,41],[35,73,42],[35,74,32],[35,74,33],[35,74,34],[35,74,35],[35,74,36],[35,74,37],[35,74,38],[35,74,39],[35,74,40],[35,74,41],[35,75,30],[35,75,31],[35,75,32],[35,75,33],[35,75,34],[35,75,35],[35,75,36],[35,75,37],[35,75,38],[35,75,39],[35,76,29],[35,76,30],[35,76,31],[35,76,32],[35,76,33],[35,76,34],[35,76,35],[35,76,36],[35,76,37],[35,76,38],[35,77,29],[35,77,30],[35,77,31],[35,77,32],[35,77,34],[35,78,29],[35,78,30],[35,78,31],[35,78,32],[35,79,30],[35,79,31],[36,57,50],[36,57,51],[36,57,52],[36,57,53],[36,58,50],[36,58,51],[36,58,52],[36,58,53],[36,59,49],[36,59,50],[36,59,51],[36,59,52],[36,59,53],[36,59,54],[36,59,55],[36,59,56],[36,60,48],[36,60,49],[36,60,50],[36,60,53],[36,60,54],[36,60,55],[36,60,56],[36,61,47],[36,61,48],[36,61,49],[36,61,50],[36,61,53],[36,61,54],[36,61,55],[36,61,56],[36,62,47],[36,62,48],[36,62,49],[36,62,50],[36,63,45],[36,63,46],[36,63,47],[36,63,48],[36,63,50],[36,63,51],[36,63,52],[36,63,53],[36,64,45],[36,64,46],[36,64,47],[36,64,48],[36,64,50],[36,64,51],[36,64,52],[36,64,53],[36,65,43],[36,65,44],[36,65,45],[36,65,46],[36,65,47],[36,65,48],[36,65,49],[36,65,50],[36,65,51],[36,65,52],[36,65,53],[36,66,43],[36,66,44],[36,66,45],[36,66,46],[36,66,47],[36,66,48],[36,66,49],[36,66,50],[36,66,51],[36,66,52],[36,66,53],[36,67,43],[36,67,44],[36,67,45],[36,67,46],[36,67,47],[36,67,48],[36,68,43],[36,68,44],[36,68,45],[36,69,45],[36,69,46],[36,70,39],[36,70,40],[36,70,41],[36,70,42],[36,70,43],[36,70,45],[36,70,46],[36,71,27],[36,71,28],[36,71,37],[36,71,38],[36,71,39],[36,71,40],[36,71,41],[36,71,42],[36,71,43],[36,71,44],[36,71,45],[36,71,46],[36,72,27],[36,72,28],[36,72,29],[36,72,30],[36,72,37],[36,72,39],[36,72,40],[36,72,41],[36,72,42],[36,72,43],[36,72,45],[36,72,46],[36,73,37],[36,73,38],[36,73,39],[36,73,40],[36,73,41],[36,73,42],[36,73,43],[36,74,32],[36,74,33],[36,74,34],[36,74,35],[36,74,36],[36,74,37],[36,74,38],[36,74,39],[36,74,40],[36,74,41],[36,75,30],[36,75,31],[36,75,32],[36,75,33],[36,75,34],[36,75,35],[36,75,36],[36,75,37],[36,75,38],[36,75,39],[36,75,40],[36,76,29],[36,76,30],[36,76,31],[36,76,32],[36,76,33],[36,76,34],[36,76,35],[36,76,36],[36,76,37],[36,76,38],[36,76,39],[36,77,29],[36,77,30],[36,77,31],[36,77,32],[36,77,33],[36,77,34],[36,78,29],[36,78,30],[36,78,31],[36,78,32],[37,57,50],[37,57,51],[37,57,52],[37,57,53],[37,57,54],[37,57,55],[37,58,50],[37,58,51],[37,58,52],[37,58,53],[37,58,54],[37,58,55],[37,58,56],[37,59,49],[37,59,50],[37,59,51],[37,59,52],[37,59,53],[37,59,54],[37,59,55],[37,59,56],[37,60,48],[37,60,49],[37,60,50],[37,60,51],[37,60,52],[37,60,53],[37,60,54],[37,60,55],[37,60,56],[37,61,47],[37,61,48],[37,61,49],[37,61,50],[37,61,51],[37,61,52],[37,61,53],[37,62,46],[37,62,47],[37,62,48],[37,62,49],[37,62,50],[37,62,51],[37,62,52],[37,62,53],[37,63,45],[37,63,46],[37,63,47],[37,63,48],[37,63,50],[37,63,51],[37,63,52],[37,63,53],[37,64,27],[37,64,28],[37,64,29],[37,64,45],[37,64,46],[37,64,47],[37,64,48],[37,64,49],[37,64,50],[37,64,51],[37,64,52],[37,64,53],[37,65,27],[37,65,28],[37,65,29],[37,65,45],[37,65,46],[37,65,47],[37,65,48],[37,65,49],[37,65,50],[37,65,51],[37,65,52],[37,65,53],[37,66,27],[37,66,28],[37,66,29],[37,66,43],[37,66,44],[37,66,45],[37,66,46],[37,66,47],[37,66,48],[37,66,49],[37,66,50],[37,66,51],[37,66,52],[37,66,53],[37,67,27],[37,67,28],[37,67,29],[37,67,43],[37,67,44],[37,67,45],[37,67,46],[37,67,47],[37,67,48],[37,67,49],[37,67,50],[37,67,51],[37,68,27],[37,68,28],[37,68,29],[37,68,43],[37,68,44],[37,68,45],[37,68,47],[37,68,48],[37,69,27],[37,69,28],[37,69,29],[37,69,30],[37,69,41],[37,69,42],[37,69,43],[37,69,44],[37,69,45],[37,70,27],[37,70,28],[37,70,29],[37,70,30],[37,70,31],[37,70,39],[37,70,40],[37,70,41],[37,70,42],[37,70,43],[37,70,44],[37,70,45],[37,70,46],[37,71,27],[37,71,28],[37,71,29],[37,71,30],[37,71,31],[37,71,32],[37,71,37],[37,71,38],[37,71,39],[37,71,40],[37,71,41],[37,71,42],[37,71,43],[37,71,44],[37,71,45],[37,71,46],[37,72,27],[37,72,28],[37,72,29],[37,72,30],[37,72,31],[37,72,32],[37,72,37],[37,72,38],[37,72,39],[37,72,40],[37,72,41],[37,72,42],[37,72,43],[37,72,44],[37,72,45],[37,72,46],[37,73,27],[37,73,28],[37,73,29],[37,73,30],[37,73,31],[37,73,32],[37,73,33],[37,73,34],[37,73,35],[37,73,36],[37,73,37],[37,73,38],[37,73,39],[37,73,40],[37,73,41],[37,73,42],[37,73,43],[37,73,44],[37,73,45],[37,74,27],[37,74,28],[37,74,29],[37,74,30],[37,74,31],[37,74,32],[37,74,33],[37,74,34],[37,74,35],[37,74,36],[37,74,37],[37,74,38],[37,74,39],[37,74,40],[37,74,41],[37,74,42],[37,74,43],[37,74,44],[37,75,29],[37,75,30],[37,75,31],[37,75,32],[37,75,33],[37,75,34],[37,75,35],[37,75,36],[37,75,37],[37,75,38],[37,75,39],[37,75,40],[37,76,29],[37,76,30],[37,76,31],[37,76,32],[37,76,33],[37,76,34],[37,76,35],[37,76,36],[37,76,37],[37,76,38],[37,76,39],[37,76,40],[37,77,29],[37,77,30],[37,77,31],[37,77,32],[37,77,33],[37,77,34],[37,77,35],[37,77,36],[37,77,37],[37,77,38],[37,78,29],[37,78,30],[37,78,31],[37,78,32],[37,78,33],[37,78,34],[37,78,35],[37,78,36],[37,78,37],[37,78,38],[37,79,31],[38,57,50],[38,57,51],[38,57,52],[38,57,53],[38,57,54],[38,57,55],[38,58,50],[38,58,51],[38,58,52],[38,58,53],[38,58,54],[38,58,55],[38,58,56],[38,59,50],[38,59,51],[38,59,52],[38,59,53],[38,59,54],[38,59,55],[38,59,56],[38,60,48],[38,60,49],[38,60,50],[38,60,51],[38,60,52],[38,60,53],[38,60,54],[38,60,55],[38,60,56],[38,61,47],[38,61,48],[38,61,49],[38,61,50],[38,61,51],[38,61,52],[38,61,53],[38,62,46],[38,62,47],[38,62,48],[38,62,49],[38,62,50],[38,62,51],[38,62,52],[38,62,53],[38,63,45],[38,63,46],[38,63,47],[38,63,48],[38,63,49],[38,63,50],[38,63,51],[38,63,52],[38,63,53],[38,64,27],[38,64,28],[38,64,29],[38,64,45],[38,64,46],[38,64,47],[38,64,48],[38,64,49],[38,64,50],[38,64,51],[38,64,52],[38,64,53],[38,65,27],[38,65,28],[38,65,29],[38,65,45],[38,65,46],[38,65,47],[38,65,48],[38,65,49],[38,65,50],[38,65,51],[38,65,52],[38,65,53],[38,66,27],[38,66,45],[38,66,46],[38,66,47],[38,66,48],[38,66,49],[38,66,50],[38,66,51],[38,66,52],[38,66,53],[38,67,27],[38,67,28],[38,67,29],[38,67,43],[38,67,45],[38,67,46],[38,67,47],[38,67,48],[38,67,49],[38,67,50],[38,67,51],[38,68,27],[38,68,28],[38,68,29],[38,68,43],[38,68,44],[38,68,45],[38,68,46],[38,68,47],[38,68,48],[38,69,27],[38,69,28],[38,69,29],[38,69,30],[38,69,41],[38,69,42],[38,69,43],[38,69,44],[38,69,45],[38,69,46],[38,69,47],[38,70,27],[38,70,28],[38,70,29],[38,70,30],[38,70,31],[38,70,40],[38,70,41],[38,70,42],[38,70,43],[38,70,44],[38,70,45],[38,70,46],[38,70,47],[38,71,27],[38,71,28],[38,71,29],[38,71,30],[38,71,31],[38,71,32],[38,71,37],[38,71,38],[38,71,39],[38,71,40],[38,71,41],[38,71,42],[38,71,43],[38,71,44],[38,71,45],[38,71,46],[38,71,47],[38,72,27],[38,72,28],[38,72,29],[38,72,30],[38,72,31],[38,72,32],[38,72,37],[38,72,38],[38,72,39],[38,72,40],[38,72,41],[38,72,42],[38,72,43],[38,72,44],[38,72,45],[38,72,46],[38,73,27],[38,73,28],[38,73,29],[38,73,30],[38,73,31],[38,73,32],[38,73,33],[38,73,34],[38,73,35],[38,73,36],[38,73,37],[38,73,38],[38,73,39],[38,73,40],[38,73,41],[38,73,42],[38,73,43],[38,73,44],[38,73,45],[38,74,27],[38,74,28],[38,74,29],[38,74,30],[38,74,31],[38,74,32],[38,74,33],[38,74,34],[38,74,35],[38,74,36],[38,74,37],[38,74,38],[38,74,39],[38,74,40],[38,74,41],[38,74,42],[38,74,43],[38,75,29],[38,75,30],[38,75,31],[38,75,32],[38,75,33],[38,75,34],[38,75,35],[38,75,36],[38,75,37],[38,75,38],[38,75,39],[38,75,40],[38,76,29],[38,76,30],[38,76,31],[38,76,32],[38,76,33],[38,76,34],[38,76,35],[38,76,36],[38,76,37],[38,76,38],[38,76,39],[38,76,40],[38,77,29],[38,77,30],[38,77,31],[38,77,32],[38,77,33],[38,77,34],[38,77,35],[38,77,36],[38,77,37],[38,77,38],[38,78,29],[38,78,30],[38,78,31],[38,78,32],[38,78,33],[38,78,34],[38,78,35],[38,78,36],[38,78,37],[38,78,38],[38,79,31],[39,57,54],[39,57,55],[39,58,50],[39,58,51],[39,58,52],[39,58,53],[39,58,54],[39,58,55],[39,58,56],[39,59,50],[39,59,51],[39,59,52],[39,59,53],[39,59,54],[39,59,55],[39,59,56],[39,60,49],[39,60,50],[39,60,51],[39,60,52],[39,60,53],[39,60,54],[39,60,55],[39,60,56],[39,61,48],[39,61,49],[39,61,50],[39,61,51],[39,61,52],[39,61,53],[39,62,47],[39,62,48],[39,62,49],[39,62,50],[39,62,51],[39,62,52],[39,62,53],[39,63,47],[39,63,48],[39,63,49],[39,63,50],[39,63,51],[39,63,52],[39,63,53],[39,64,47],[39,64,48],[39,64,49],[39,64,50],[39,64,51],[39,64,52],[39,64,53],[39,65,27],[39,65,45],[39,65,46],[39,65,47],[39,65,48],[39,65,49],[39,65,50],[39,65,51],[39,65,52],[39,65,53],[39,66,27],[39,66,45],[39,66,46],[39,66,47],[39,66,48],[39,66,49],[39,66,50],[39,66,51],[39,66,52],[39,66,53],[39,67,27],[39,67,28],[39,67,29],[39,67,45],[39,67,46],[39,67,47],[39,67,48],[39,67,49],[39,67,50],[39,67,51],[39,68,27],[39,68,28],[39,68,29],[39,68,45],[39,68,46],[39,68,47],[39,68,48],[39,69,27],[39,69,28],[39,69,29],[39,69,30],[39,69,41],[39,69,42],[39,69,43],[39,69,44],[39,69,45],[39,69,46],[39,69,47],[39,70,27],[39,70,28],[39,70,29],[39,70,30],[39,70,41],[39,70,42],[39,70,43],[39,70,44],[39,70,45],[39,70,46],[39,70,47],[39,71,27],[39,71,28],[39,71,29],[39,71,30],[39,71,31],[39,71,32],[39,71,37],[39,71,38],[39,71,39],[39,71,40],[39,71,41],[39,71,42],[39,71,43],[39,71,44],[39,71,45],[39,71,46],[39,71,47],[39,72,27],[39,72,28],[39,72,29],[39,72,30],[39,72,31],[39,72,32],[39,72,37],[39,72,38],[39,72,39],[39,72,40],[39,72,41],[39,72,42],[39,72,43],[39,72,44],[39,72,45],[39,72,46],[39,73,27],[39,73,28],[39,73,29],[39,73,30],[39,73,31],[39,73,32],[39,73,33],[39,73,34],[39,73,35],[39,73,36],[39,73,37],[39,73,38],[39,73,39],[39,73,40],[39,73,41],[39,73,42],[39,73,43],[39,73,44],[39,74,27],[39,74,28],[39,74,29],[39,74,30],[39,74,31],[39,74,32],[39,74,33],[39,74,34],[39,74,35],[39,74,36],[39,74,37],[39,74,38],[39,74,39],[39,74,40],[39,74,41],[39,75,29],[39,75,30],[39,75,31],[39,75,32],[39,75,33],[39,75,34],[39,75,35],[39,75,36],[39,75,37],[39,75,38],[39,75,39],[39,75,40],[39,76,29],[39,76,30],[39,76,31],[39,76,32],[39,76,33],[39,76,34],[39,76,35],[39,76,36],[39,76,37],[39,76,38],[39,76,39],[39,77,29],[39,77,30],[39,77,31],[39,77,32],[39,77,33],[39,77,34],[39,77,35],[39,77,36],[39,77,37],[39,77,38],[39,78,31],[39,78,32],[39,78,33],[39,78,34],[39,78,35],[39,78,36],[39,78,37],[39,78,38],[39,79,31],[40,57,53],[40,57,54],[40,58,50],[40,58,51],[40,58,52],[40,58,53],[40,58,54],[40,58,55],[40,58,56],[40,59,50],[40,59,51],[40,59,52],[40,59,53],[40,59,54],[40,59,55],[40,59,56],[40,60,50],[40,60,51],[40,60,52],[40,60,53],[40,60,54],[40,60,55],[40,60,56],[40,61,48],[40,61,49],[40,61,50],[40,61,51],[40,61,52],[40,61,53],[40,62,48],[40,62,49],[40,62,50],[40,62,51],[40,62,52],[40,62,53],[40,63,47],[40,63,48],[40,63,49],[40,63,50],[40,63,51],[40,63,52],[40,63,53],[40,64,47],[40,64,48],[40,64,49],[40,64,50],[40,64,51],[40,64,52],[40,64,53],[40,65,45],[40,65,46],[40,65,47],[40,65,48],[40,65,49],[40,65,50],[40,65,51],[40,65,52],[40,65,53],[40,66,45],[40,66,46],[40,66,47],[40,66,48],[40,66,49],[40,66,50],[40,66,51],[40,67,27],[40,67,28],[40,67,29],[40,67,45],[40,67,46],[40,67,47],[40,67,48],[40,67,49],[40,67,50],[40,67,51],[40,68,27],[40,68,28],[40,68,29],[40,68,45],[40,68,46],[40,68,47],[40,68,48],[40,69,27],[40,69,28],[40,69,29],[40,69,30],[40,69,41],[40,69,42],[40,69,43],[40,69,45],[40,69,46],[40,69,47],[40,69,48],[40,70,27],[40,70,28],[40,70,29],[40,70,30],[40,70,41],[40,70,42],[40,70,43],[40,70,45],[40,70,46],[40,71,27],[40,71,28],[40,71,29],[40,71,30],[40,71,31],[40,71,32],[40,71,37],[40,71,38],[40,71,39],[40,71,40],[40,71,41],[40,71,42],[40,71,43],[40,71,44],[40,71,45],[40,71,46],[40,72,27],[40,72,28],[40,72,29],[40,72,30],[40,72,31],[40,72,32],[40,72,37],[40,72,38],[40,72,39],[40,72,40],[40,72,41],[40,72,42],[40,72,43],[40,72,44],[40,72,45],[40,72,46],[40,73,27],[40,73,28],[40,73,29],[40,73,30],[40,73,31],[40,73,32],[40,73,36],[40,73,37],[40,73,38],[40,73,39],[40,73,40],[40,73,41],[40,73,42],[40,74,27],[40,74,28],[40,74,29],[40,74,30],[40,74,31],[40,74,32],[40,74,33],[40,74,34],[40,74,35],[40,74,36],[40,74,37],[40,74,38],[40,74,39],[40,74,40],[40,75,29],[40,75,30],[40,75,31],[40,75,32],[40,75,33],[40,75,34],[40,75,35],[40,75,36],[40,75,37],[40,75,38],[40,75,39],[40,75,40],[40,76,29],[40,76,30],[40,76,31],[40,76,32],[40,76,33],[40,76,34],[40,76,35],[40,76,36],[40,76,37],[40,76,38],[40,77,30],[40,77,31],[40,77,32],[40,77,33],[40,77,34],[40,77,35],[40,77,36],[40,77,37],[40,77,38],[40,78,31],[40,78,32],[40,78,33],[40,78,34],[40,78,35],[40,78,36],[40,78,37],[40,78,38],[41,57,52],[41,57,53],[41,57,54],[41,57,55],[41,58,50],[41,58,51],[41,58,52],[41,58,53],[41,58,54],[41,58,55],[41,58,56],[41,59,49],[41,59,50],[41,59,51],[41,59,52],[41,59,53],[41,59,54],[41,59,55],[41,59,56],[41,60,49],[41,60,50],[41,60,51],[41,60,52],[41,60,53],[41,60,54],[41,60,55],[41,60,56],[41,61,47],[41,61,48],[41,61,49],[41,61,50],[41,61,51],[41,61,52],[41,61,53],[41,62,46],[41,62,47],[41,62,48],[41,62,49],[41,62,50],[41,62,51],[41,62,52],[41,62,53],[41,63,46],[41,63,47],[41,63,48],[41,63,49],[41,63,50],[41,63,51],[41,63,52],[41,63,53],[41,64,47],[41,64,48],[41,64,49],[41,64,50],[41,64,51],[41,64,52],[41,64,53],[41,65,28],[41,65,29],[41,65,45],[41,65,46],[41,65,47],[41,65,48],[41,65,49],[41,65,50],[41,65,51],[41,65,52],[41,65,53],[41,66,27],[41,66,28],[41,66,29],[41,66,45],[41,66,46],[41,66,47],[41,66,48],[41,66,49],[41,66,50],[41,66,51],[41,66,52],[41,66,53],[41,67,27],[41,67,28],[41,67,29],[41,67,45],[41,67,46],[41,67,47],[41,67,48],[41,67,49],[41,67,50],[41,67,51],[41,68,27],[41,68,28],[41,68,29],[41,68,45],[41,68,46],[41,68,47],[41,68,48],[41,69,27],[41,69,28],[41,69,29],[41,69,30],[41,69,41],[41,69,42],[41,69,43],[41,69,44],[41,69,45],[41,69,46],[41,69,47],[41,69,48],[41,70,27],[41,70,28],[41,70,29],[41,70,30],[41,70,31],[41,70,41],[41,70,42],[41,70,43],[41,70,44],[41,70,45],[41,70,46],[41,70,47],[41,71,27],[41,71,28],[41,71,29],[41,71,30],[41,71,31],[41,71,32],[41,71,37],[41,71,38],[41,71,39],[41,71,40],[41,71,41],[41,71,42],[41,71,43],[41,71,44],[41,71,45],[41,71,46],[41,71,47],[41,72,27],[41,72,28],[41,72,29],[41,72,30],[41,72,31],[41,72,32],[41,72,37],[41,72,38],[41,72,39],[41,72,40],[41,72,41],[41,72,42],[41,72,43],[41,72,44],[41,72,45],[41,72,46],[41,73,27],[41,73,28],[41,73,29],[41,73,30],[41,73,31],[41,73,32],[41,73,33],[41,73,34],[41,73,35],[41,73,36],[41,73,37],[41,73,38],[41,73,39],[41,73,40],[41,73,41],[41,73,42],[41,74,27],[41,74,28],[41,74,29],[41,74,30],[41,74,31],[41,74,32],[41,74,33],[41,74,34],[41,74,35],[41,74,36],[41,74,37],[41,74,38],[41,74,39],[41,74,40],[41,74,41],[41,74,42],[41,75,29],[41,75,30],[41,75,31],[41,75,32],[41,75,33],[41,75,34],[41,75,35],[41,75,36],[41,75,37],[41,75,38],[41,75,39],[41,75,40],[41,76,29],[41,76,30],[41,76,31],[41,76,32],[41,76,33],[41,76,34],[41,76,35],[41,76,36],[41,76,37],[41,76,38],[41,76,39],[41,76,40],[41,77,29],[41,77,30],[41,77,31],[41,77,32],[41,77,33],[41,77,34],[41,77,35],[41,77,36],[41,77,37],[41,77,38],[41,78,31],[41,78,32],[41,78,33],[41,78,34],[41,78,35],[41,78,36],[41,78,37],[41,78,38],[41,79,31],[42,57,50],[42,57,51],[42,57,52],[42,57,53],[42,57,54],[42,57,55],[42,58,50],[42,58,51],[42,58,52],[42,58,53],[42,58,54],[42,58,55],[42,58,56],[42,59,49],[42,59,50],[42,59,51],[42,59,52],[42,59,53],[42,59,54],[42,59,55],[42,59,56],[42,60,49],[42,60,50],[42,60,51],[42,60,52],[42,60,53],[42,60,54],[42,60,55],[42,60,56],[42,61,47],[42,61,48],[42,61,49],[42,61,50],[42,61,51],[42,61,52],[42,61,53],[42,62,46],[42,62,47],[42,62,48],[42,62,49],[42,62,50],[42,62,51],[42,62,52],[42,62,53],[42,63,46],[42,63,47],[42,63,48],[42,63,49],[42,63,50],[42,63,51],[42,63,52],[42,63,53],[42,64,27],[42,64,28],[42,64,29],[42,64,45],[42,64,46],[42,64,47],[42,64,48],[42,64,49],[42,64,50],[42,64,51],[42,64,52],[42,64,53],[42,65,27],[42,65,28],[42,65,29],[42,65,45],[42,65,46],[42,65,47],[42,65,48],[42,65,49],[42,65,50],[42,65,51],[42,65,52],[42,65,53],[42,66,27],[42,66,28],[42,66,29],[42,66,45],[42,66,46],[42,66,47],[42,66,48],[42,66,49],[42,66,50],[42,66,51],[42,66,52],[42,66,53],[42,67,27],[42,67,28],[42,67,29],[42,67,45],[42,67,46],[42,67,47],[42,67,48],[42,67,49],[42,67,50],[42,67,51],[42,68,27],[42,68,45],[42,68,46],[42,68,47],[42,68,48],[42,69,27],[42,69,28],[42,69,29],[42,69,30],[42,69,41],[42,69,42],[42,69,43],[42,69,44],[42,69,45],[42,69,46],[42,69,47],[42,69,48],[42,70,27],[42,70,28],[42,70,29],[42,70,30],[42,70,31],[42,70,41],[42,70,42],[42,70,43],[42,70,44],[42,70,45],[42,70,46],[42,70,47],[42,71,27],[42,71,28],[42,71,29],[42,71,30],[42,71,31],[42,71,32],[42,71,37],[42,71,38],[42,71,39],[42,71,40],[42,71,41],[42,71,42],[42,71,43],[42,71,44],[42,71,45],[42,71,46],[42,71,47],[42,72,27],[42,72,28],[42,72,29],[42,72,30],[42,72,31],[42,72,32],[42,72,37],[42,72,38],[42,72,39],[42,72,40],[42,72,41],[42,72,42],[42,72,43],[42,72,44],[42,72,45],[42,72,46],[42,73,27],[42,73,28],[42,73,29],[42,73,30],[42,73,31],[42,73,32],[42,73,33],[42,73,34],[42,73,35],[42,73,36],[42,73,37],[42,73,38],[42,73,39],[42,73,40],[42,73,41],[42,73,42],[42,73,43],[42,73,44],[42,73,45],[42,74,27],[42,74,28],[42,74,29],[42,74,30],[42,74,31],[42,74,32],[42,74,33],[42,74,34],[42,74,35],[42,74,36],[42,74,37],[42,74,38],[42,74,39],[42,74,40],[42,74,41],[42,74,42],[42,74,43],[42,75,29],[42,75,30],[42,75,31],[42,75,32],[42,75,33],[42,75,34],[42,75,35],[42,75,36],[42,75,37],[42,75,38],[42,75,39],[42,75,40],[42,76,29],[42,76,30],[42,76,31],[42,76,32],[42,76,33],[42,76,34],[42,76,35],[42,76,36],[42,76,37],[42,76,38],[42,76,39],[42,76,40],[42,77,29],[42,77,30],[42,77,31],[42,77,32],[42,77,33],[42,77,34],[42,77,35],[42,77,36],[42,77,37],[42,77,38],[42,78,29],[42,78,30],[42,78,31],[42,78,32],[42,78,33],[42,78,34],[42,78,35],[42,78,36],[42,78,37],[42,78,38],[43,57,50],[43,57,51],[43,57,52],[43,57,53],[43,57,54],[43,57,55],[43,58,50],[43,58,51],[43,58,52],[43,58,53],[43,58,54],[43,58,55],[43,58,56],[43,59,49],[43,59,50],[43,59,51],[43,59,52],[43,59,53],[43,59,54],[43,59,55],[43,59,56],[43,60,48],[43,60,49],[43,60,50],[43,60,51],[43,60,52],[43,60,53],[43,60,54],[43,60,55],[43,60,56],[43,61,47],[43,61,48],[43,61,49],[43,61,50],[43,62,46],[43,62,47],[43,62,48],[43,62,49],[43,62,50],[43,62,51],[43,62,52],[43,62,53],[43,63,45],[43,63,46],[43,63,47],[43,63,48],[43,63,50],[43,63,51],[43,63,52],[43,63,53],[43,64,27],[43,64,28],[43,64,29],[43,64,45],[43,64,46],[43,64,47],[43,64,48],[43,64,50],[43,64,51],[43,64,52],[43,64,53],[43,65,27],[43,65,28],[43,65,29],[43,65,45],[43,65,46],[43,65,47],[43,65,48],[43,65,49],[43,65,50],[43,65,51],[43,65,52],[43,65,53],[43,66,27],[43,66,28],[43,66,29],[43,66,43],[43,66,44],[43,66,45],[43,66,46],[43,66,47],[43,66,48],[43,66,49],[43,66,50],[43,66,51],[43,66,52],[43,66,53],[43,67,27],[43,67,28],[43,67,29],[43,67,43],[43,67,44],[43,67,45],[43,67,46],[43,67,47],[43,67,48],[43,67,49],[43,67,50],[43,67,51],[43,68,43],[43,68,44],[43,68,45],[43,68,46],[43,68,47],[43,68,48],[43,69,41],[43,69,42],[43,69,43],[43,69,44],[43,69,45],[43,69,46],[43,70,27],[43,70,28],[43,70,29],[43,70,30],[43,70,31],[43,70,39],[43,70,40],[43,70,41],[43,70,42],[43,70,43],[43,70,44],[43,70,45],[43,70,46],[43,70,47],[43,71,27],[43,71,28],[43,71,29],[43,71,30],[43,71,31],[43,71,37],[43,71,38],[43,71,39],[43,71,40],[43,71,41],[43,71,42],[43,71,43],[43,71,44],[43,71,45],[43,71,46],[43,71,47],[43,72,27],[43,72,28],[43,72,29],[43,72,30],[43,72,31],[43,72,32],[43,72,37],[43,72,38],[43,72,39],[43,72,40],[43,72,41],[43,72,42],[43,72,43],[43,72,44],[43,72,45],[43,72,46],[43,73,27],[43,73,28],[43,73,29],[43,73,30],[43,73,31],[43,73,32],[43,73,33],[43,73,34],[43,73,35],[43,73,36],[43,73,37],[43,73,38],[43,73,39],[43,73,40],[43,73,41],[43,73,42],[43,73,43],[43,73,44],[43,73,45],[43,74,27],[43,74,28],[43,74,29],[43,74,30],[43,74,31],[43,74,32],[43,74,33],[43,74,34],[43,74,35],[43,74,36],[43,74,37],[43,74,38],[43,74,39],[43,74,40],[43,74,41],[43,74,42],[43,74,43],[43,75,30],[43,75,31],[43,75,32],[43,75,33],[43,75,34],[43,75,35],[43,75,36],[43,75,37],[43,75,38],[43,75,39],[43,75,40],[43,76,29],[43,76,30],[43,76,31],[43,76,32],[43,76,33],[43,76,34],[43,76,35],[43,76,36],[43,76,37],[43,76,38],[43,76,39],[43,76,40],[43,77,29],[43,77,30],[43,77,31],[43,77,32],[43,77,33],[43,77,34],[43,77,35],[43,77,36],[43,77,37],[43,77,38],[43,78,29],[43,78,30],[43,78,31],[43,78,32],[43,78,33],[43,78,34],[43,78,35],[43,78,36],[44,57,50],[44,57,51],[44,57,52],[44,57,53],[44,58,50],[44,58,51],[44,58,52],[44,58,53],[44,58,54],[44,58,55],[44,58,56],[44,59,49],[44,59,50],[44,59,51],[44,59,52],[44,59,53],[44,59,54],[44,59,55],[44,59,56],[44,60,48],[44,60,49],[44,60,50],[44,60,52],[44,60,53],[44,60,54],[44,60,55],[44,60,56],[44,61,47],[44,61,48],[44,61,49],[44,61,50],[44,62,46],[44,62,47],[44,62,48],[44,62,49],[44,62,50],[44,63,45],[44,63,46],[44,63,47],[44,63,48],[44,63,50],[44,63,51],[44,63,52],[44,63,53],[44,64,27],[44,64,28],[44,64,29],[44,64,45],[44,64,46],[44,64,47],[44,64,48],[44,64,50],[44,64,51],[44,64,52],[44,64,53],[44,65,27],[44,65,28],[44,65,29],[44,65,43],[44,65,45],[44,65,46],[44,65,47],[44,65,48],[44,65,49],[44,65,50],[44,65,51],[44,65,52],[44,65,53],[44,66,27],[44,66,28],[44,66,29],[44,66,43],[44,66,44],[44,66,45],[44,66,46],[44,66,47],[44,66,48],[44,66,49],[44,66,50],[44,66,51],[44,66,52],[44,66,53],[44,67,43],[44,67,44],[44,67,45],[44,67,47],[44,67,48],[44,68,43],[44,68,44],[44,68,45],[44,68,48],[44,69,43],[44,69,44],[44,69,45],[44,69,46],[44,69,47],[44,70,27],[44,70,28],[44,70,29],[44,70,30],[44,70,39],[44,70,40],[44,70,41],[44,70,45],[44,70,46],[44,70,47],[44,71,27],[44,71,28],[44,71,29],[44,71,30],[44,71,37],[44,71,38],[44,71,39],[44,71,40],[44,71,41],[44,71,42],[44,71,43],[44,71,44],[44,71,45],[44,71,46],[44,72,27],[44,72,28],[44,72,29],[44,72,30],[44,72,37],[44,72,39],[44,72,40],[44,72,41],[44,72,42],[44,72,43],[44,72,44],[44,72,45],[44,72,46],[44,73,27],[44,73,28],[44,73,29],[44,73,37],[44,73,38],[44,73,39],[44,73,40],[44,73,41],[44,73,42],[44,73,43],[44,73,44],[44,73,45],[44,74,27],[44,74,28],[44,74,29],[44,74,32],[44,74,33],[44,74,34],[44,74,35],[44,74,36],[44,74,37],[44,74,38],[44,74,39],[44,74,40],[44,74,41],[44,74,42],[44,74,43],[44,74,44],[44,75,30],[44,75,31],[44,75,32],[44,75,33],[44,75,34],[44,75,35],[44,75,36],[44,75,37],[44,75,38],[44,75,39],[44,75,40],[44,76,29],[44,76,30],[44,76,31],[44,76,32],[44,76,33],[44,76,34],[44,76,35],[44,76,36],[44,76,37],[44,76,38],[44,76,40],[44,77,29],[44,77,30],[44,77,31],[44,77,32],[44,77,33],[44,77,34],[44,77,35],[44,77,36],[44,77,37],[44,77,38],[44,78,29],[44,78,30],[44,78,31],[44,78,32],[45,57,50],[45,57,51],[45,57,52],[45,57,53],[45,58,50],[45,58,51],[45,58,52],[45,58,53],[45,59,49],[45,59,50],[45,59,51],[45,59,52],[45,59,53],[45,59,54],[45,59,55],[45,59,56],[45,60,48],[45,60,49],[45,60,50],[45,60,53],[45,60,54],[45,60,55],[45,60,56],[45,61,47],[45,61,48],[45,61,49],[45,61,50],[45,61,53],[45,61,54],[45,61,55],[45,61,56],[45,62,46],[45,62,47],[45,62,48],[45,62,49],[45,62,50],[45,63,45],[45,63,46],[45,63,47],[45,63,48],[45,63,50],[45,63,51],[45,63,52],[45,63,53],[45,64,46],[45,64,47],[45,64,48],[45,64,50],[45,64,51],[45,64,52],[45,64,53],[45,65,43],[45,65,46],[45,65,47],[45,65,48],[45,65,49],[45,65,50],[45,65,51],[45,65,52],[45,65,53],[45,66,43],[45,66,44],[45,66,45],[45,66,47],[45,66,48],[45,66,49],[45,66,50],[45,66,51],[45,66,52],[45,66,53],[45,67,43],[45,67,44],[45,67,45],[45,67,47],[45,67,48],[45,68,43],[45,68,44],[45,68,45],[45,68,46],[45,69,45],[45,69,46],[45,69,47],[45,70,27],[45,70,28],[45,70,29],[45,70,30],[45,70,39],[45,70,40],[45,70,41],[45,70,42],[45,70,43],[45,70,45],[45,70,46],[45,70,47],[45,71,27],[45,71,28],[45,71,29],[45,71,30],[45,71,39],[45,71,40],[45,71,41],[45,71,42],[45,71,43],[45,71,44],[45,71,45],[45,71,46],[45,72,27],[45,72,28],[45,72,29],[45,72,30],[45,72,37],[45,72,38],[45,72,39],[45,72,40],[45,72,41],[45,72,42],[45,72,43],[45,72,45],[45,72,46],[45,73,37],[45,73,38],[45,73,39],[45,73,40],[45,73,41],[45,73,42],[45,74,34],[45,74,35],[45,74,36],[45,74,37],[45,74,38],[45,74,39],[45,74,40],[45,74,41],[45,75,30],[45,75,31],[45,75,32],[45,75,34],[45,75,35],[45,75,36],[45,75,37],[45,75,38],[45,75,39],[45,75,40],[45,76,29],[45,76,30],[45,76,31],[45,76,32],[45,76,34],[45,76,35],[45,76,36],[45,76,37],[45,76,38],[45,77,29],[45,77,30],[45,77,31],[45,77,32],[45,78,29],[45,78,30],[45,78,31],[45,78,32],[46,57,50],[46,57,51],[46,57,52],[46,57,53],[46,58,50],[46,58,51],[46,58,52],[46,58,53],[46,59,48],[46,59,49],[46,59,50],[46,59,51],[46,59,52],[46,59,53],[46,59,54],[46,59,55],[46,59,56],[46,60,48],[46,60,49],[46,60,50],[46,60,53],[46,60,54],[46,60,55],[46,60,56],[46,61,48],[46,61,49],[46,61,50],[46,61,53],[46,61,54],[46,61,55],[46,61,56],[46,62,45],[46,62,46],[46,62,47],[46,62,48],[46,62,49],[46,62,50],[46,63,45],[46,63,46],[46,63,47],[46,63,48],[46,63,51],[46,63,52],[46,63,53],[46,64,45],[46,64,46],[46,64,47],[46,64,48],[46,64,50],[46,64,51],[46,64,52],[46,64,53],[46,65,43],[46,65,44],[46,65,45],[46,65,46],[46,65,47],[46,65,48],[46,65,49],[46,65,50],[46,65,51],[46,65,52],[46,65,53],[46,66,43],[46,66,44],[46,66,45],[46,66,46],[46,66,47],[46,66,48],[46,66,49],[46,66,50],[46,66,51],[46,66,52],[46,66,53],[46,67,43],[46,67,44],[46,67,45],[46,67,46],[46,67,47],[46,67,48],[46,67,49],[46,68,45],[46,68,46],[46,68,47],[46,69,45],[46,69,46],[46,69,47],[46,70,27],[46,70,28],[46,70,29],[46,70,30],[46,70,41],[46,70,45],[46,70,46],[46,71,27],[46,71,28],[46,71,29],[46,71,30],[46,71,39],[46,71,40],[46,71,41],[46,71,42],[46,71,43],[46,71,44],[46,71,45],[46,72,27],[46,72,28],[46,72,29],[46,72,30],[46,72,37],[46,72,38],[46,72,39],[46,72,40],[46,72,41],[46,72,42],[46,72,43],[46,72,44],[46,73,37],[46,73,38],[46,73,39],[46,73,40],[46,73,41],[46,73,42],[46,74,34],[46,74,35],[46,74,36],[46,74,37],[46,74,38],[46,74,39],[46,74,40],[46,75,29],[46,75,32],[46,75,34],[46,75,35],[46,75,36],[46,75,37],[46,75,38],[46,76,29],[46,76,30],[46,76,31],[46,76,32],[46,76,34],[46,76,35],[46,76,36],[46,76,37],[46,76,38],[46,77,29],[46,77,30],[46,77,31],[46,77,32],[46,78,29],[46,78,30],[46,78,31],[46,78,32],[46,79,30],[46,79,31],[47,57,50],[47,57,51],[47,57,52],[47,57,53],[47,58,50],[47,58,51],[47,58,52],[47,58,53],[47,59,48],[47,59,49],[47,59,50],[47,59,51],[47,59,52],[47,59,53],[47,59,54],[47,59,55],[47,59,56],[47,60,48],[47,60,49],[47,60,50],[47,60,53],[47,60,54],[47,60,55],[47,60,56],[47,61,49],[47,61,50],[47,61,53],[47,61,54],[47,61,55],[47,61,56],[47,62,47],[47,62,48],[47,62,49],[47,62,50],[47,63,47],[47,63,48],[47,64,47],[47,64,48],[47,65,47],[47,65,48],[47,65,49],[47,65,50],[47,66,46],[47,66,47],[47,66,48],[47,66,49],[47,66,50],[47,66,51],[47,67,45],[47,67,46],[47,67,47],[47,67,48],[47,67,49],[47,67,50],[47,67,51],[47,68,45],[47,68,46],[47,68,47],[47,69,41],[47,69,42],[47,69,43],[47,69,45],[47,69,46],[47,69,47],[47,70,41],[47,70,42],[47,70,43],[47,70,44],[47,70,45],[47,71,39],[47,71,40],[47,71,41],[47,71,42],[47,71,43],[47,71,44],[47,72,37],[47,72,38],[47,72,39],[47,72,40],[47,72,41],[47,72,42],[47,72,43],[47,72,44],[47,72,45],[47,73,37],[47,73,38],[47,73,39],[47,73,40],[47,74,34],[47,74,35],[47,74,36],[47,74,37],[47,74,38],[47,74,39],[47,74,40],[47,75,29],[47,75,32],[47,75,34],[47,75,35],[47,75,36],[47,75,37],[47,75,38],[47,76,29],[47,76,30],[47,76,31],[47,76,32],[47,76,34],[47,76,35],[47,76,36],[47,76,37],[47,76,38],[47,77,29],[47,77,30],[47,77,31],[47,77,32],[47,77,36],[47,77,37],[47,77,38],[47,78,30],[47,78,31],[48,57,50],[48,57,51],[48,57,52],[48,58,53],[48,58,54],[48,58,55],[48,58,56],[48,59,48],[48,59,49],[48,59,50],[48,59,53],[48,59,54],[48,59,55],[48,59,56],[48,60,48],[48,60,49],[48,60,50],[48,60,53],[48,60,54],[48,60,55],[48,60,56],[48,61,48],[48,61,49],[48,61,50],[48,61,53],[48,61,54],[48,61,55],[48,62,48],[48,62,49],[48,62,50],[48,63,48],[48,64,47],[48,64,48],[48,65,48],[48,65,49],[48,65,50],[48,66,47],[48,66,48],[48,66,49],[48,66,50],[48,66,51],[48,67,47],[48,67,48],[48,67,49],[48,67,50],[48,67,51],[48,68,45],[48,68,46],[48,69,45],[48,69,46],[48,70,41],[48,70,42],[48,70,43],[48,70,44],[48,70,45],[48,70,46],[48,71,39],[48,71,40],[48,71,41],[48,71,42],[48,71,43],[48,71,44],[48,71,45],[48,72,38],[48,72,39],[48,72,40],[48,72,41],[48,72,42],[48,72,43],[48,72,44],[48,72,45],[48,73,37],[48,73,38],[48,73,39],[48,73,40],[48,73,41],[48,74,34],[48,74,35],[48,74,36],[48,74,37],[48,74,38],[48,74,39],[48,74,40],[48,75,29],[48,75,30],[48,75,31],[48,75,32],[48,75,34],[48,75,35],[48,75,36],[48,75,37],[48,75,38],[48,76,29],[48,76,30],[48,76,31],[48,76,34],[48,76,35],[48,76,36],[48,76,37],[48,76,38],[48,77,30],[48,77,31],[48,77,36],[48,77,37],[48,77,38],[49,58,53],[49,58,54],[49,58,55],[49,58,56],[49,59,53],[49,59,54],[49,59,55],[49,59,56],[49,60,53],[49,60,54],[49,60,55],[49,60,56],[49,70,41],[49,70,42],[49,70,43],[49,70,44],[49,70,45],[49,71,41],[49,71,42],[49,71,43],[49,71,44],[49,71,45],[49,72,39],[49,72,40],[49,72,41],[49,72,42],[49,72,43],[49,72,44],[49,72,45],[49,73,37],[49,73,38],[49,73,39],[49,73,40],[49,73,41],[49,74,38],[49,74,39],[49,74,40],[49,75,36],[49,75,37],[49,75,38],[49,76,36],[49,76,37],[49,76,38],[49,77,36],[49,77,37],[49,77,38],[50,58,53],[50,58,54],[50,58,55],[50,59,53],[50,59,54],[50,59,55],[50,60,53],[50,60,54],[50,60,55],[50,72,39],[50,72,40],[50,72,41],[50,72,43],[50,73,38],[50,73,39],[50,73,40],[50,73,41],[50,74,38],[50,74,39],[50,74,40]],"colors":[[0,0,0,1]],"centers":[[30,72,41],[30,73,38],[30,73,39],[31,58,53],[31,58,54],[31,58,55],[31,58,56],[31,59,53],[31,59,54],[31,59,55],[31,59,56],[31,60,53],[31,60,54],[31,60,55],[31,60,56],[31,70,41],[31,70,42],[31,70,43],[31,70,44],[31,71,39],[31,71,40],[31,71,41],[31,71,42],[31,71,43],[31,71,44],[31,72,39],[31,72,40],[31,72,41],[31,72,42],[31,72,43],[31,72,44],[31,72,45],[31,73,37],[31,73,38],[31,73,39],[31,73,40],[31,73,41],[31,73,42],[31,74,38],[31,74,39],[31,74,40],[31,75,36],[31,75,37],[31,75,38],[31,76,36],[31,76,37],[31,76,38],[31,77,36],[31,77,37],[31,77,38],[32,57,50],[32,57,51],[32,57,52],[32,57,53],[32,58,51],[32,58,53],[32,58,54],[32,58,55],[32,58,56],[32,59,53],[32,59,54],[32,59,55],[32,59,56],[32,60,48],[32,60,49],[32,60,50],[32,60,53],[32,60,54],[32,60,55],[32,60,56],[32,61,48],[32,61,49],[32,61,50],[32,62,48],[32,62,49],[32,62,50],[32,65,48],[32,65,49],[32,65,50],[32,66,47],[32,66,48],[32,66,49],[32,66,50],[32,66,51],[32,67,47],[32,67,48],[32,67,49],[32,67,50],[32,67,51],[32,70,41],[32,70,42],[32,70,43],[32,70,44],[32,71,39],[32,71,40],[32,71,41],[32,71,42],[32,71,43],[32,71,44],[32,72,38],[32,72,39],[32,72,40],[32,72,41],[32,72,42],[32,72,43],[32,72,44],[32,72,45],[32,73,37],[32,73,38],[32,73,39],[32,73,40],[32,73,41],[32,74,37],[32,74,38],[32,74,39],[32,74,40],[32,74,41],[32,75,29],[32,75,30],[32,75,31],[32,75,32],[32,75,36],[32,76,29],[32,76,30],[32,76,31],[32,76,34],[32,76,35],[32,76,36],[32,77,30],[32,77,31],[33,57,50],[33,57,51],[33,57,52],[33,57,53],[33,58,50],[33,58,51],[33,58,52],[33,58,53],[33,59,48],[33,59,49],[33,59,50],[33,59,51],[33,59,52],[33,59,53],[33,59,54],[33,59,55],[33,59,56],[33,60,48],[33,60,49],[33,60,50],[33,60,53],[33,60,54],[33,60,55],[33,60,56],[33,61,48],[33,61,49],[33,61,50],[33,61,53],[33,61,54],[33,61,55],[33,61,56],[33,62,47],[33,62,48],[33,62,49],[33,62,50],[33,63,47],[33,63,48],[33,64,47],[33,64,48],[33,65,48],[33,65,49],[33,65,50],[33,66,47],[33,66,48],[33,66,49],[33,66,50],[33,66,51],[33,67,45],[33,67,46],[33,67,47],[33,67,48],[33,67,49],[33,67,50],[33,67,51],[33,68,45],[33,68,46],[33,68,47],[33,69,45],[33,69,46],[33,69,47],[33,70,41],[33,70,42],[33,70,43],[33,70,44],[33,70,45],[33,70,46],[33,71,39],[33,71,40],[33,71,41],[33,71,42],[33,71,43],[33,71,44],[33,72,37],[33,72,38],[33,72,39],[33,72,40],[33,72,41],[33,72,42],[33,72,43],[33,72,44],[33,72,45],[33,73,37],[33,73,38],[33,73,39],[33,73,40],[33,74,34],[33,74,35],[33,74,36],[33,74,37],[33,74,38],[33,74,39],[33,74,40],[33,75,29],[33,75,30],[33,75,31],[33,75,32],[33,75,34],[33,75,35],[33,75,36],[33,75,37],[33,75,38],[33,76,29],[33,76,30],[33,76,31],[33,76,32],[33,76,34],[33,76,35],[33,76,36],[33,76,37],[33,76,38],[33,77,29],[33,77,30],[33,77,31],[33,77,32],[33,78,29],[33,78,30],[33,78,31],[33,78,32],[34,57,50],[34,57,51],[34,57,52],[34,57,53],[34,58,50],[34,58,51],[34,58,52],[34,58,53],[34,59,48],[34,59,49],[34,59,50],[34,59,51],[34,59,52],[34,59,53],[34,59,54],[34,59,55],[34,59,56],[34,60,48],[34,60,49],[34,60,50],[34,60,53],[34,60,54],[34,60,55],[34,60,56],[34,61,48],[34,61,49],[34,61,50],[34,61,53],[34,61,54],[34,61,55],[34,61,56],[34,62,47],[34,62,48],[34,62,49],[34,62,50],[34,63,47],[34,63,48],[34,64,47],[34,64,48],[34,65,46],[34,65,47],[34,65,48],[34,65,49],[34,65,50],[34,65,51],[34,65,52],[34,65,53],[34,66,46],[34,66,47],[34,66,48],[34,66,49],[34,66,50],[34,66,51],[34,66,52],[34,67,45],[34,67,46],[34,67,47],[34,67,48],[34,67,49],[34,67,50],[34,67,51],[34,68,45],[34,68,46],[34,69,45],[34,69,46],[34,70,41],[34,70,42],[34,70,43],[34,70,44],[34,70,45],[34,70,46],[34,71,39],[34,71,40],[34,71,41],[34,71,42],[34,71,43],[34,71,44],[34,71,45],[34,72,37],[34,72,38],[34,72,39],[34,72,40],[34,72,41],[34,72,42],[34,72,43],[34,72,44],[34,72,45],[34,73,37],[34,73,38],[34,73,39],[34,73,40],[34,74,34],[34,74,35],[34,74,36],[34,74,37],[34,74,40],[34,75,29],[34,75,32],[34,75,34],[34,75,35],[34,75,36],[34,75,37],[34,75,38],[34,76,29],[34,76,30],[34,76,31],[34,76,32],[34,76,34],[34,76,35],[34,76,36],[34,76,37],[34,76,38],[34,77,29],[34,77,30],[34,77,31],[34,77,32],[34,78,29],[34,78,30],[34,78,31],[34,78,32],[34,79,30],[34,79,31],[35,57,50],[35,57,51],[35,57,52],[35,57,53],[35,58,50],[35,58,51],[35,58,52],[35,58,53],[35,59,48],[35,59,49],[35,59,50],[35,59,51],[35,59,52],[35,59,53],[35,59,54],[35,59,55],[35,59,56],[35,60,48],[35,60,49],[35,60,50],[35,60,53],[35,60,54],[35,60,55],[35,60,56],[35,61,48],[35,61,49],[35,61,50],[35,61,53],[35,61,54],[35,61,55],[35,61,56],[35,62,46],[35,62,47],[35,62,48],[35,62,49],[35,62,50],[35,63,45],[35,63,46],[35,63,47],[35,63,48],[35,63,50],[35,63,51],[35,63,52],[35,63,53],[35,64,45],[35,64,46],[35,64,47],[35,64,48],[35,64,50],[35,64,51],[35,64,52],[35,64,53],[35,65,43],[35,65,44],[35,65,45],[35,65,46],[35,65,47],[35,65,48],[35,65,49],[35,65,50],[35,65,51],[35,65,52],[35,65,53],[35,66,43],[35,66,44],[35,66,47],[35,66,48],[35,66,49],[35,66,50],[35,66,51],[35,66,52],[35,67,43],[35,67,44],[35,67,45],[35,67,46],[35,67,47],[35,67,48],[35,68,43],[35,68,44],[35,68,45],[35,68,46],[35,69,45],[35,69,46],[35,69,47],[35,70,27],[35,70,28],[35,70,29],[35,70,30],[35,70,41],[35,70,45],[35,70,46],[35,70,47],[35,71,27],[35,71,28],[35,71,29],[35,71,30],[35,71,38],[35,71,39],[35,71,40],[35,71,41],[35,71,42],[35,71,43],[35,71,44],[35,71,45],[35,71,46],[35,72,27],[35,72,28],[35,72,29],[35,72,30],[35,72,37],[35,72,38],[35,72,39],[35,72,40],[35,72,41],[35,72,42],[35,72,43],[35,73,37],[35,73,38],[35,73,39],[35,73,40],[35,73,41],[35,73,42],[35,74,32],[35,74,33],[35,74,34],[35,74,35],[35,74,36],[35,74,37],[35,74,38],[35,74,39],[35,74,40],[35,74,41],[35,75,30],[35,75,31],[35,75,32],[35,75,33],[35,75,34],[35,75,35],[35,75,36],[35,75,37],[35,75,38],[35,75,39],[35,76,29],[35,76,30],[35,76,31],[35,76,32],[35,76,33],[35,76,34],[35,76,35],[35,76,36],[35,76,37],[35,76,38],[35,77,29],[35,77,30],[35,77,31],[35,77,32],[35,77,34],[35,78,29],[35,78,30],[35,78,31],[35,78,32],[35,79,30],[35,79,31],[36,57,50],[36,57,51],[36,57,52],[36,57,53],[36,58,50],[36,58,51],[36,58,52],[36,58,53],[36,59,49],[36,59,50],[36,59,51],[36,59,52],[36,59,53],[36,59,54],[36,59,55],[36,59,56],[36,60,48],[36,60,49],[36,60,50],[36,60,53],[36,60,54],[36,60,55],[36,60,56],[36,61,47],[36,61,48],[36,61,49],[36,61,50],[36,61,53],[36,61,54],[36,61,55],[36,61,56],[36,62,47],[36,62,48],[36,62,49],[36,62,50],[36,63,45],[36,63,46],[36,63,47],[36,63,48],[36,63,50],[36,63,51],[36,63,52],[36,63,53],[36,64,45],[36,64,46],[36,64,47],[36,64,48],[36,64,50],[36,64,51],[36,64,52],[36,64,53],[36,65,43],[36,65,44],[36,65,45],[36,65,46],[36,65,47],[36,65,48],[36,65,49],[36,65,50],[36,65,51],[36,65,52],[36,65,53],[36,66,43],[36,66,44],[36,66,45],[36,66,46],[36,66,47],[36,66,48],[36,66,49],[36,66,50],[36,66,51],[36,66,52],[36,66,53],[36,67,43],[36,67,44],[36,67,45],[36,67,46],[36,67,47],[36,67,48],[36,68,43],[36,68,44],[36,68,45],[36,69,45],[36,69,46],[36,70,39],[36,70,40],[36,70,41],[36,70,42],[36,70,43],[36,70,45],[36,70,46],[36,71,27],[36,71,28],[36,71,37],[36,71,38],[36,71,39],[36,71,40],[36,71,41],[36,71,42],[36,71,43],[36,71,44],[36,71,45],[36,71,46],[36,72,27],[36,72,28],[36,72,29],[36,72,30],[36,72,37],[36,72,39],[36,72,40],[36,72,41],[36,72,42],[36,72,43],[36,72,45],[36,72,46],[36,73,37],[36,73,38],[36,73,39],[36,73,40],[36,73,41],[36,73,42],[36,73,43],[36,74,32],[36,74,33],[36,74,34],[36,74,35],[36,74,36],[36,74,37],[36,74,38],[36,74,39],[36,74,40],[36,74,41],[36,75,30],[36,75,31],[36,75,32],[36,75,33],[36,75,34],[36,75,35],[36,75,36],[36,75,37],[36,75,38],[36,75,39],[36,75,40],[36,76,29],[36,76,30],[36,76,31],[36,76,32],[36,76,33],[36,76,34],[36,76,35],[36,76,36],[36,76,37],[36,76,38],[36,76,39],[36,77,29],[36,77,30],[36,77,31],[36,77,32],[36,77,33],[36,77,34],[36,78,29],[36,78,30],[36,78,31],[36,78,32],[37,57,50],[37,57,51],[37,57,52],[37,57,53],[37,57,54],[37,57,55],[37,58,50],[37,58,51],[37,58,52],[37,58,53],[37,58,54],[37,58,55],[37,58,56],[37,59,49],[37,59,50],[37,59,51],[37,59,52],[37,59,53],[37,59,54],[37,59,55],[37,59,56],[37,60,48],[37,60,49],[37,60,50],[37,60,51],[37,60,52],[37,60,53],[37,60,54],[37,60,55],[37,60,56],[37,61,47],[37,61,48],[37,61,49],[37,61,50],[37,61,51],[37,61,52],[37,61,53],[37,62,46],[37,62,47],[37,62,48],[37,62,49],[37,62,50],[37,62,51],[37,62,52],[37,62,53],[37,63,45],[37,63,46],[37,63,47],[37,63,48],[37,63,50],[37,63,51],[37,63,52],[37,63,53],[37,64,27],[37,64,28],[37,64,29],[37,64,45],[37,64,46],[37,64,47],[37,64,48],[37,64,49],[37,64,50],[37,64,51],[37,64,52],[37,64,53],[37,65,27],[37,65,28],[37,65,29],[37,65,45],[37,65,46],[37,65,47],[37,65,48],[37,65,49],[37,65,50],[37,65,51],[37,65,52],[37,65,53],[37,66,27],[37,66,28],[37,66,29],[37,66,43],[37,66,44],[37,66,45],[37,66,46],[37,66,47],[37,66,48],[37,66,49],[37,66,50],[37,66,51],[37,66,52],[37,66,53],[37,67,27],[37,67,28],[37,67,29],[37,67,43],[37,67,44],[37,67,45],[37,67,46],[37,67,47],[37,67,48],[37,67,49],[37,67,50],[37,67,51],[37,68,27],[37,68,28],[37,68,29],[37,68,43],[37,68,44],[37,68,45],[37,68,47],[37,68,48],[37,69,27],[37,69,28],[37,69,29],[37,69,30],[37,69,41],[37,69,42],[37,69,43],[37,69,44],[37,69,45],[37,70,27],[37,70,28],[37,70,29],[37,70,30],[37,70,31],[37,70,39],[37,70,40],[37,70,41],[37,70,42],[37,70,43],[37,70,44],[37,70,45],[37,70,46],[37,71,27],[37,71,28],[37,71,29],[37,71,30],[37,71,31],[37,71,32],[37,71,37],[37,71,38],[37,71,39],[37,71,40],[37,71,41],[37,71,42],[37,71,43],[37,71,44],[37,71,45],[37,71,46],[37,72,27],[37,72,28],[37,72,29],[37,72,30],[37,72,31],[37,72,32],[37,72,37],[37,72,38],[37,72,39],[37,72,40],[37,72,41],[37,72,42],[37,72,43],[37,72,44],[37,72,45],[37,72,46],[37,73,27],[37,73,28],[37,73,29],[37,73,30],[37,73,31],[37,73,32],[37,73,33],[37,73,34],[37,73,35],[37,73,36],[37,73,37],[37,73,38],[37,73,39],[37,73,40],[37,73,41],[37,73,42],[37,73,43],[37,73,44],[37,73,45],[37,74,27],[37,74,28],[37,74,29],[37,74,30],[37,74,31],[37,74,32],[37,74,33],[37,74,34],[37,74,35],[37,74,36],[37,74,37],[37,74,38],[37,74,39],[37,74,40],[37,74,41],[37,74,42],[37,74,43],[37,74,44],[37,75,29],[37,75,30],[37,75,31],[37,75,32],[37,75,33],[37,75,34],[37,75,35],[37,75,36],[37,75,37],[37,75,38],[37,75,39],[37,75,40],[37,76,29],[37,76,30],[37,76,31],[37,76,32],[37,76,33],[37,76,34],[37,76,35],[37,76,36],[37,76,37],[37,76,38],[37,76,39],[37,76,40],[37,77,29],[37,77,30],[37,77,31],[37,77,32],[37,77,33],[37,77,34],[37,77,35],[37,77,36],[37,77,37],[37,77,38],[37,78,29],[37,78,30],[37,78,31],[37,78,32],[37,78,33],[37,78,34],[37,78,35],[37,78,36],[37,78,37],[37,78,38],[37,79,31],[38,57,50],[38,57,51],[38,57,52],[38,57,53],[38,57,54],[38,57,55],[38,58,50],[38,58,51],[38,58,52],[38,58,53],[38,58,54],[38,58,55],[38,58,56],[38,59,50],[38,59,51],[38,59,52],[38,59,53],[38,59,54],[38,59,55],[38,59,56],[38,60,48],[38,60,49],[38,60,50],[38,60,51],[38,60,52],[38,60,53],[38,60,54],[38,60,55],[38,60,56],[38,61,47],[38,61,48],[38,61,49],[38,61,50],[38,61,51],[38,61,52],[38,61,53],[38,62,46],[38,62,47],[38,62,48],[38,62,49],[38,62,50],[38,62,51],[38,62,52],[38,62,53],[38,63,45],[38,63,46],[38,63,47],[38,63,48],[38,63,49],[38,63,50],[38,63,51],[38,63,52],[38,63,53],[38,64,27],[38,64,28],[38,64,29],[38,64,45],[38,64,46],[38,64,47],[38,64,48],[38,64,49],[38,64,50],[38,64,51],[38,64,52],[38,64,53],[38,65,27],[38,65,28],[38,65,29],[38,65,45],[38,65,46],[38,65,47],[38,65,48],[38,65,49],[38,65,50],[38,65,51],[38,65,52],[38,65,53],[38,66,27],[38,66,45],[38,66,46],[38,66,47],[38,66,48],[38,66,49],[38,66,50],[38,66,51],[38,66,52],[38,66,53],[38,67,27],[38,67,28],[38,67,29],[38,67,43],[38,67,45],[38,67,46],[38,67,47],[38,67,48],[38,67,49],[38,67,50],[38,67,51],[38,68,27],[38,68,28],[38,68,29],[38,68,43],[38,68,44],[38,68,45],[38,68,46],[38,68,47],[38,68,48],[38,69,27],[38,69,28],[38,69,29],[38,69,30],[38,69,41],[38,69,42],[38,69,43],[38,69,44],[38,69,45],[38,69,46],[38,69,47],[38,70,27],[38,70,28],[38,70,29],[38,70,30],[38,70,31],[38,70,40],[38,70,41],[38,70,42],[38,70,43],[38,70,44],[38,70,45],[38,70,46],[38,70,47],[38,71,27],[38,71,28],[38,71,29],[38,71,30],[38,71,31],[38,71,32],[38,71,37],[38,71,38],[38,71,39],[38,71,40],[38,71,41],[38,71,42],[38,71,43],[38,71,44],[38,71,45],[38,71,46],[38,71,47],[38,72,27],[38,72,28],[38,72,29],[38,72,30],[38,72,31],[38,72,32],[38,72,37],[38,72,38],[38,72,39],[38,72,40],[38,72,41],[38,72,42],[38,72,43],[38,72,44],[38,72,45],[38,72,46],[38,73,27],[38,73,28],[38,73,29],[38,73,30],[38,73,31],[38,73,32],[38,73,33],[38,73,34],[38,73,35],[38,73,36],[38,73,37],[38,73,38],[38,73,39],[38,73,40],[38,73,41],[38,73,42],[38,73,43],[38,73,44],[38,73,45],[38,74,27],[38,74,28],[38,74,29],[38,74,30],[38,74,31],[38,74,32],[38,74,33],[38,74,34],[38,74,35],[38,74,36],[38,74,37],[38,74,38],[38,74,39],[38,74,40],[38,74,41],[38,74,42],[38,74,43],[38,75,29],[38,75,30],[38,75,31],[38,75,32],[38,75,33],[38,75,34],[38,75,35],[38,75,36],[38,75,37],[38,75,38],[38,75,39],[38,75,40],[38,76,29],[38,76,30],[38,76,31],[38,76,32],[38,76,33],[38,76,34],[38,76,35],[38,76,36],[38,76,37],[38,76,38],[38,76,39],[38,76,40],[38,77,29],[38,77,30],[38,77,31],[38,77,32],[38,77,33],[38,77,34],[38,77,35],[38,77,36],[38,77,37],[38,77,38],[38,78,29],[38,78,30],[38,78,31],[38,78,32],[38,78,33],[38,78,34],[38,78,35],[38,78,36],[38,78,37],[38,78,38],[38,79,31],[39,57,54],[39,57,55],[39,58,50],[39,58,51],[39,58,52],[39,58,53],[39,58,54],[39,58,55],[39,58,56],[39,59,50],[39,59,51],[39,59,52],[39,59,53],[39,59,54],[39,59,55],[39,59,56],[39,60,49],[39,60,50],[39,60,51],[39,60,52],[39,60,53],[39,60,54],[39,60,55],[39,60,56],[39,61,48],[39,61,49],[39,61,50],[39,61,51],[39,61,52],[39,61,53],[39,62,47],[39,62,48],[39,62,49],[39,62,50],[39,62,51],[39,62,52],[39,62,53],[39,63,47],[39,63,48],[39,63,49],[39,63,50],[39,63,51],[39,63,52],[39,63,53],[39,64,47],[39,64,48],[39,64,49],[39,64,50],[39,64,51],[39,64,52],[39,64,53],[39,65,27],[39,65,45],[39,65,46],[39,65,47],[39,65,48],[39,65,49],[39,65,50],[39,65,51],[39,65,52],[39,65,53],[39,66,27],[39,66,45],[39,66,46],[39,66,47],[39,66,48],[39,66,49],[39,66,50],[39,66,51],[39,66,52],[39,66,53],[39,67,27],[39,67,28],[39,67,29],[39,67,45],[39,67,46],[39,67,47],[39,67,48],[39,67,49],[39,67,50],[39,67,51],[39,68,27],[39,68,28],[39,68,29],[39,68,45],[39,68,46],[39,68,47],[39,68,48],[39,69,27],[39,69,28],[39,69,29],[39,69,30],[39,69,41],[39,69,42],[39,69,43],[39,69,44],[39,69,45],[39,69,46],[39,69,47],[39,70,27],[39,70,28],[39,70,29],[39,70,30],[39,70,41],[39,70,42],[39,70,43],[39,70,44],[39,70,45],[39,70,46],[39,70,47],[39,71,27],[39,71,28],[39,71,29],[39,71,30],[39,71,31],[39,71,32],[39,71,37],[39,71,38],[39,71,39],[39,71,40],[39,71,41],[39,71,42],[39,71,43],[39,71,44],[39,71,45],[39,71,46],[39,71,47],[39,72,27],[39,72,28],[39,72,29],[39,72,30],[39,72,31],[39,72,32],[39,72,37],[39,72,38],[39,72,39],[39,72,40],[39,72,41],[39,72,42],[39,72,43],[39,72,44],[39,72,45],[39,72,46],[39,73,27],[39,73,28],[39,73,29],[39,73,30],[39,73,31],[39,73,32],[39,73,33],[39,73,34],[39,73,35],[39,73,36],[39,73,37],[39,73,38],[39,73,39],[39,73,40],[39,73,41],[39,73,42],[39,73,43],[39,73,44],[39,74,27],[39,74,28],[39,74,29],[39,74,30],[39,74,31],[39,74,32],[39,74,33],[39,74,34],[39,74,35],[39,74,36],[39,74,37],[39,74,38],[39,74,39],[39,74,40],[39,74,41],[39,75,29],[39,75,30],[39,75,31],[39,75,32],[39,75,33],[39,75,34],[39,75,35],[39,75,36],[39,75,37],[39,75,38],[39,75,39],[39,75,40],[39,76,29],[39,76,30],[39,76,31],[39,76,32],[39,76,33],[39,76,34],[39,76,35],[39,76,36],[39,76,37],[39,76,38],[39,76,39],[39,77,29],[39,77,30],[39,77,31],[39,77,32],[39,77,33],[39,77,34],[39,77,35],[39,77,36],[39,77,37],[39,77,38],[39,78,31],[39,78,32],[39,78,33],[39,78,34],[39,78,35],[39,78,36],[39,78,37],[39,78,38],[39,79,31],[40,57,53],[40,57,54],[40,58,50],[40,58,51],[40,58,52],[40,58,53],[40,58,54],[40,58,55],[40,58,56],[40,59,50],[40,59,51],[40,59,52],[40,59,53],[40,59,54],[40,59,55],[40,59,56],[40,60,50],[40,60,51],[40,60,52],[40,60,53],[40,60,54],[40,60,55],[40,60,56],[40,61,48],[40,61,49],[40,61,50],[40,61,51],[40,61,52],[40,61,53],[40,62,48],[40,62,49],[40,62,50],[40,62,51],[40,62,52],[40,62,53],[40,63,47],[40,63,48],[40,63,49],[40,63,50],[40,63,51],[40,63,52],[40,63,53],[40,64,47],[40,64,48],[40,64,49],[40,64,50],[40,64,51],[40,64,52],[40,64,53],[40,65,45],[40,65,46],[40,65,47],[40,65,48],[40,65,49],[40,65,50],[40,65,51],[40,65,52],[40,65,53],[40,66,45],[40,66,46],[40,66,47],[40,66,48],[40,66,49],[40,66,50],[40,66,51],[40,67,27],[40,67,28],[40,67,29],[40,67,45],[40,67,46],[40,67,47],[40,67,48],[40,67,49],[40,67,50],[40,67,51],[40,68,27],[40,68,28],[40,68,29],[40,68,45],[40,68,46],[40,68,47],[40,68,48],[40,69,27],[40,69,28],[40,69,29],[40,69,30],[40,69,41],[40,69,42],[40,69,43],[40,69,45],[40,69,46],[40,69,47],[40,69,48],[40,70,27],[40,70,28],[40,70,29],[40,70,30],[40,70,41],[40,70,42],[40,70,43],[40,70,45],[40,70,46],[40,71,27],[40,71,28],[40,71,29],[40,71,30],[40,71,31],[40,71,32],[40,71,37],[40,71,38],[40,71,39],[40,71,40],[40,71,41],[40,71,42],[40,71,43],[40,71,44],[40,71,45],[40,71,46],[40,72,27],[40,72,28],[40,72,29],[40,72,30],[40,72,31],[40,72,32],[40,72,37],[40,72,38],[40,72,39],[40,72,40],[40,72,41],[40,72,42],[40,72,43],[40,72,44],[40,72,45],[40,72,46],[40,73,27],[40,73,28],[40,73,29],[40,73,30],[40,73,31],[40,73,32],[40,73,36],[40,73,37],[40,73,38],[40,73,39],[40,73,40],[40,73,41],[40,73,42],[40,74,27],[40,74,28],[40,74,29],[40,74,30],[40,74,31],[40,74,32],[40,74,33],[40,74,34],[40,74,35],[40,74,36],[40,74,37],[40,74,38],[40,74,39],[40,74,40],[40,75,29],[40,75,30],[40,75,31],[40,75,32],[40,75,33],[40,75,34],[40,75,35],[40,75,36],[40,75,37],[40,75,38],[40,75,39],[40,75,40],[40,76,29],[40,76,30],[40,76,31],[40,76,32],[40,76,33],[40,76,34],[40,76,35],[40,76,36],[40,76,37],[40,76,38],[40,77,30],[40,77,31],[40,77,32],[40,77,33],[40,77,34],[40,77,35],[40,77,36],[40,77,37],[40,77,38],[40,78,31],[40,78,32],[40,78,33],[40,78,34],[40,78,35],[40,78,36],[40,78,37],[40,78,38],[41,57,52],[41,57,53],[41,57,54],[41,57,55],[41,58,50],[41,58,51],[41,58,52],[41,58,53],[41,58,54],[41,58,55],[41,58,56],[41,59,49],[41,59,50],[41,59,51],[41,59,52],[41,59,53],[41,59,54],[41,59,55],[41,59,56],[41,60,49],[41,60,50],[41,60,51],[41,60,52],[41,60,53],[41,60,54],[41,60,55],[41,60,56],[41,61,47],[41,61,48],[41,61,49],[41,61,50],[41,61,51],[41,61,52],[41,61,53],[41,62,46],[41,62,47],[41,62,48],[41,62,49],[41,62,50],[41,62,51],[41,62,52],[41,62,53],[41,63,46],[41,63,47],[41,63,48],[41,63,49],[41,63,50],[41,63,51],[41,63,52],[41,63,53],[41,64,47],[41,64,48],[41,64,49],[41,64,50],[41,64,51],[41,64,52],[41,64,53],[41,65,28],[41,65,29],[41,65,45],[41,65,46],[41,65,47],[41,65,48],[41,65,49],[41,65,50],[41,65,51],[41,65,52],[41,65,53],[41,66,27],[41,66,28],[41,66,29],[41,66,45],[41,66,46],[41,66,47],[41,66,48],[41,66,49],[41,66,50],[41,66,51],[41,66,52],[41,66,53],[41,67,27],[41,67,28],[41,67,29],[41,67,45],[41,67,46],[41,67,47],[41,67,48],[41,67,49],[41,67,50],[41,67,51],[41,68,27],[41,68,28],[41,68,29],[41,68,45],[41,68,46],[41,68,47],[41,68,48],[41,69,27],[41,69,28],[41,69,29],[41,69,30],[41,69,41],[41,69,42],[41,69,43],[41,69,44],[41,69,45],[41,69,46],[41,69,47],[41,69,48],[41,70,27],[41,70,28],[41,70,29],[41,70,30],[41,70,31],[41,70,41],[41,70,42],[41,70,43],[41,70,44],[41,70,45],[41,70,46],[41,70,47],[41,71,27],[41,71,28],[41,71,29],[41,71,30],[41,71,31],[41,71,32],[41,71,37],[41,71,38],[41,71,39],[41,71,40],[41,71,41],[41,71,42],[41,71,43],[41,71,44],[41,71,45],[41,71,46],[41,71,47],[41,72,27],[41,72,28],[41,72,29],[41,72,30],[41,72,31],[41,72,32],[41,72,37],[41,72,38],[41,72,39],[41,72,40],[41,72,41],[41,72,42],[41,72,43],[41,72,44],[41,72,45],[41,72,46],[41,73,27],[41,73,28],[41,73,29],[41,73,30],[41,73,31],[41,73,32],[41,73,33],[41,73,34],[41,73,35],[41,73,36],[41,73,37],[41,73,38],[41,73,39],[41,73,40],[41,73,41],[41,73,42],[41,74,27],[41,74,28],[41,74,29],[41,74,30],[41,74,31],[41,74,32],[41,74,33],[41,74,34],[41,74,35],[41,74,36],[41,74,37],[41,74,38],[41,74,39],[41,74,40],[41,74,41],[41,74,42],[41,75,29],[41,75,30],[41,75,31],[41,75,32],[41,75,33],[41,75,34],[41,75,35],[41,75,36],[41,75,37],[41,75,38],[41,75,39],[41,75,40],[41,76,29],[41,76,30],[41,76,31],[41,76,32],[41,76,33],[41,76,34],[41,76,35],[41,76,36],[41,76,37],[41,76,38],[41,76,39],[41,76,40],[41,77,29],[41,77,30],[41,77,31],[41,77,32],[41,77,33],[41,77,34],[41,77,35],[41,77,36],[41,77,37],[41,77,38],[41,78,31],[41,78,32],[41,78,33],[41,78,34],[41,78,35],[41,78,36],[41,78,37],[41,78,38],[41,79,31],[42,57,50],[42,57,51],[42,57,52],[42,57,53],[42,57,54],[42,57,55],[42,58,50],[42,58,51],[42,58,52],[42,58,53],[42,58,54],[42,58,55],[42,58,56],[42,59,49],[42,59,50],[42,59,51],[42,59,52],[42,59,53],[42,59,54],[42,59,55],[42,59,56],[42,60,49],[42,60,50],[42,60,51],[42,60,52],[42,60,53],[42,60,54],[42,60,55],[42,60,56],[42,61,47],[42,61,48],[42,61,49],[42,61,50],[42,61,51],[42,61,52],[42,61,53],[42,62,46],[42,62,47],[42,62,48],[42,62,49],[42,62,50],[42,62,51],[42,62,52],[42,62,53],[42,63,46],[42,63,47],[42,63,48],[42,63,49],[42,63,50],[42,63,51],[42,63,52],[42,63,53],[42,64,27],[42,64,28],[42,64,29],[42,64,45],[42,64,46],[42,64,47],[42,64,48],[42,64,49],[42,64,50],[42,64,51],[42,64,52],[42,64,53],[42,65,27],[42,65,28],[42,65,29],[42,65,45],[42,65,46],[42,65,47],[42,65,48],[42,65,49],[42,65,50],[42,65,51],[42,65,52],[42,65,53],[42,66,27],[42,66,28],[42,66,29],[42,66,45],[42,66,46],[42,66,47],[42,66,48],[42,66,49],[42,66,50],[42,66,51],[42,66,52],[42,66,53],[42,67,27],[42,67,28],[42,67,29],[42,67,45],[42,67,46],[42,67,47],[42,67,48],[42,67,49],[42,67,50],[42,67,51],[42,68,27],[42,68,45],[42,68,46],[42,68,47],[42,68,48],[42,69,27],[42,69,28],[42,69,29],[42,69,30],[42,69,41],[42,69,42],[42,69,43],[42,69,44],[42,69,45],[42,69,46],[42,69,47],[42,69,48],[42,70,27],[42,70,28],[42,70,29],[42,70,30],[42,70,31],[42,70,41],[42,70,42],[42,70,43],[42,70,44],[42,70,45],[42,70,46],[42,70,47],[42,71,27],[42,71,28],[42,71,29],[42,71,30],[42,71,31],[42,71,32],[42,71,37],[42,71,38],[42,71,39],[42,71,40],[42,71,41],[42,71,42],[42,71,43],[42,71,44],[42,71,45],[42,71,46],[42,71,47],[42,72,27],[42,72,28],[42,72,29],[42,72,30],[42,72,31],[42,72,32],[42,72,37],[42,72,38],[42,72,39],[42,72,40],[42,72,41],[42,72,42],[42,72,43],[42,72,44],[42,72,45],[42,72,46],[42,73,27],[42,73,28],[42,73,29],[42,73,30],[42,73,31],[42,73,32],[42,73,33],[42,73,34],[42,73,35],[42,73,36],[42,73,37],[42,73,38],[42,73,39],[42,73,40],[42,73,41],[42,73,42],[42,73,43],[42,73,44],[42,73,45],[42,74,27],[42,74,28],[42,74,29],[42,74,30],[42,74,31],[42,74,32],[42,74,33],[42,74,34],[42,74,35],[42,74,36],[42,74,37],[42,74,38],[42,74,39],[42,74,40],[42,74,41],[42,74,42],[42,74,43],[42,75,29],[42,75,30],[42,75,31],[42,75,32],[42,75,33],[42,75,34],[42,75,35],[42,75,36],[42,75,37],[42,75,38],[42,75,39],[42,75,40],[42,76,29],[42,76,30],[42,76,31],[42,76,32],[42,76,33],[42,76,34],[42,76,35],[42,76,36],[42,76,37],[42,76,38],[42,76,39],[42,76,40],[42,77,29],[42,77,30],[42,77,31],[42,77,32],[42,77,33],[42,77,34],[42,77,35],[42,77,36],[42,77,37],[42,77,38],[42,78,29],[42,78,30],[42,78,31],[42,78,32],[42,78,33],[42,78,34],[42,78,35],[42,78,36],[42,78,37],[42,78,38],[43,57,50],[43,57,51],[43,57,52],[43,57,53],[43,57,54],[43,57,55],[43,58,50],[43,58,51],[43,58,52],[43,58,53],[43,58,54],[43,58,55],[43,58,56],[43,59,49],[43,59,50],[43,59,51],[43,59,52],[43,59,53],[43,59,54],[43,59,55],[43,59,56],[43,60,48],[43,60,49],[43,60,50],[43,60,51],[43,60,52],[43,60,53],[43,60,54],[43,60,55],[43,60,56],[43,61,47],[43,61,48],[43,61,49],[43,61,50],[43,62,46],[43,62,47],[43,62,48],[43,62,49],[43,62,50],[43,62,51],[43,62,52],[43,62,53],[43,63,45],[43,63,46],[43,63,47],[43,63,48],[43,63,50],[43,63,51],[43,63,52],[43,63,53],[43,64,27],[43,64,28],[43,64,29],[43,64,45],[43,64,46],[43,64,47],[43,64,48],[43,64,50],[43,64,51],[43,64,52],[43,64,53],[43,65,27],[43,65,28],[43,65,29],[43,65,45],[43,65,46],[43,65,47],[43,65,48],[43,65,49],[43,65,50],[43,65,51],[43,65,52],[43,65,53],[43,66,27],[43,66,28],[43,66,29],[43,66,43],[43,66,44],[43,66,45],[43,66,46],[43,66,47],[43,66,48],[43,66,49],[43,66,50],[43,66,51],[43,66,52],[43,66,53],[43,67,27],[43,67,28],[43,67,29],[43,67,43],[43,67,44],[43,67,45],[43,67,46],[43,67,47],[43,67,48],[43,67,49],[43,67,50],[43,67,51],[43,68,43],[43,68,44],[43,68,45],[43,68,46],[43,68,47],[43,68,48],[43,69,41],[43,69,42],[43,69,43],[43,69,44],[43,69,45],[43,69,46],[43,70,27],[43,70,28],[43,70,29],[43,70,30],[43,70,31],[43,70,39],[43,70,40],[43,70,41],[43,70,42],[43,70,43],[43,70,44],[43,70,45],[43,70,46],[43,70,47],[43,71,27],[43,71,28],[43,71,29],[43,71,30],[43,71,31],[43,71,37],[43,71,38],[43,71,39],[43,71,40],[43,71,41],[43,71,42],[43,71,43],[43,71,44],[43,71,45],[43,71,46],[43,71,47],[43,72,27],[43,72,28],[43,72,29],[43,72,30],[43,72,31],[43,72,32],[43,72,37],[43,72,38],[43,72,39],[43,72,40],[43,72,41],[43,72,42],[43,72,43],[43,72,44],[43,72,45],[43,72,46],[43,73,27],[43,73,28],[43,73,29],[43,73,30],[43,73,31],[43,73,32],[43,73,33],[43,73,34],[43,73,35],[43,73,36],[43,73,37],[43,73,38],[43,73,39],[43,73,40],[43,73,41],[43,73,42],[43,73,43],[43,73,44],[43,73,45],[43,74,27],[43,74,28],[43,74,29],[43,74,30],[43,74,31],[43,74,32],[43,74,33],[43,74,34],[43,74,35],[43,74,36],[43,74,37],[43,74,38],[43,74,39],[43,74,40],[43,74,41],[43,74,42],[43,74,43],[43,75,30],[43,75,31],[43,75,32],[43,75,33],[43,75,34],[43,75,35],[43,75,36],[43,75,37],[43,75,38],[43,75,39],[43,75,40],[43,76,29],[43,76,30],[43,76,31],[43,76,32],[43,76,33],[43,76,34],[43,76,35],[43,76,36],[43,76,37],[43,76,38],[43,76,39],[43,76,40],[43,77,29],[43,77,30],[43,77,31],[43,77,32],[43,77,33],[43,77,34],[43,77,35],[43,77,36],[43,77,37],[43,77,38],[43,78,29],[43,78,30],[43,78,31],[43,78,32],[43,78,33],[43,78,34],[43,78,35],[43,78,36],[44,57,50],[44,57,51],[44,57,52],[44,57,53],[44,58,50],[44,58,51],[44,58,52],[44,58,53],[44,58,54],[44,58,55],[44,58,56],[44,59,49],[44,59,50],[44,59,51],[44,59,52],[44,59,53],[44,59,54],[44,59,55],[44,59,56],[44,60,48],[44,60,49],[44,60,50],[44,60,52],[44,60,53],[44,60,54],[44,60,55],[44,60,56],[44,61,47],[44,61,48],[44,61,49],[44,61,50],[44,62,46],[44,62,47],[44,62,48],[44,62,49],[44,62,50],[44,63,45],[44,63,46],[44,63,47],[44,63,48],[44,63,50],[44,63,51],[44,63,52],[44,63,53],[44,64,27],[44,64,28],[44,64,29],[44,64,45],[44,64,46],[44,64,47],[44,64,48],[44,64,50],[44,64,51],[44,64,52],[44,64,53],[44,65,27],[44,65,28],[44,65,29],[44,65,43],[44,65,45],[44,65,46],[44,65,47],[44,65,48],[44,65,49],[44,65,50],[44,65,51],[44,65,52],[44,65,53],[44,66,27],[44,66,28],[44,66,29],[44,66,43],[44,66,44],[44,66,45],[44,66,46],[44,66,47],[44,66,48],[44,66,49],[44,66,50],[44,66,51],[44,66,52],[44,66,53],[44,67,43],[44,67,44],[44,67,45],[44,67,47],[44,67,48],[44,68,43],[44,68,44],[44,68,45],[44,68,48],[44,69,43],[44,69,44],[44,69,45],[44,69,46],[44,69,47],[44,70,27],[44,70,28],[44,70,29],[44,70,30],[44,70,39],[44,70,40],[44,70,41],[44,70,45],[44,70,46],[44,70,47],[44,71,27],[44,71,28],[44,71,29],[44,71,30],[44,71,37],[44,71,38],[44,71,39],[44,71,40],[44,71,41],[44,71,42],[44,71,43],[44,71,44],[44,71,45],[44,71,46],[44,72,27],[44,72,28],[44,72,29],[44,72,30],[44,72,37],[44,72,39],[44,72,40],[44,72,41],[44,72,42],[44,72,43],[44,72,44],[44,72,45],[44,72,46],[44,73,27],[44,73,28],[44,73,29],[44,73,37],[44,73,38],[44,73,39],[44,73,40],[44,73,41],[44,73,42],[44,73,43],[44,73,44],[44,73,45],[44,74,27],[44,74,28],[44,74,29],[44,74,32],[44,74,33],[44,74,34],[44,74,35],[44,74,36],[44,74,37],[44,74,38],[44,74,39],[44,74,40],[44,74,41],[44,74,42],[44,74,43],[44,74,44],[44,75,30],[44,75,31],[44,75,32],[44,75,33],[44,75,34],[44,75,35],[44,75,36],[44,75,37],[44,75,38],[44,75,39],[44,75,40],[44,76,29],[44,76,30],[44,76,31],[44,76,32],[44,76,33],[44,76,34],[44,76,35],[44,76,36],[44,76,37],[44,76,38],[44,76,40],[44,77,29],[44,77,30],[44,77,31],[44,77,32],[44,77,33],[44,77,34],[44,77,35],[44,77,36],[44,77,37],[44,77,38],[44,78,29],[44,78,30],[44,78,31],[44,78,32],[45,57,50],[45,57,51],[45,57,52],[45,57,53],[45,58,50],[45,58,51],[45,58,52],[45,58,53],[45,59,49],[45,59,50],[45,59,51],[45,59,52],[45,59,53],[45,59,54],[45,59,55],[45,59,56],[45,60,48],[45,60,49],[45,60,50],[45,60,53],[45,60,54],[45,60,55],[45,60,56],[45,61,47],[45,61,48],[45,61,49],[45,61,50],[45,61,53],[45,61,54],[45,61,55],[45,61,56],[45,62,46],[45,62,47],[45,62,48],[45,62,49],[45,62,50],[45,63,45],[45,63,46],[45,63,47],[45,63,48],[45,63,50],[45,63,51],[45,63,52],[45,63,53],[45,64,46],[45,64,47],[45,64,48],[45,64,50],[45,64,51],[45,64,52],[45,64,53],[45,65,43],[45,65,46],[45,65,47],[45,65,48],[45,65,49],[45,65,50],[45,65,51],[45,65,52],[45,65,53],[45,66,43],[45,66,44],[45,66,45],[45,66,47],[45,66,48],[45,66,49],[45,66,50],[45,66,51],[45,66,52],[45,66,53],[45,67,43],[45,67,44],[45,67,45],[45,67,47],[45,67,48],[45,68,43],[45,68,44],[45,68,45],[45,68,46],[45,69,45],[45,69,46],[45,69,47],[45,70,27],[45,70,28],[45,70,29],[45,70,30],[45,70,39],[45,70,40],[45,70,41],[45,70,42],[45,70,43],[45,70,45],[45,70,46],[45,70,47],[45,71,27],[45,71,28],[45,71,29],[45,71,30],[45,71,39],[45,71,40],[45,71,41],[45,71,42],[45,71,43],[45,71,44],[45,71,45],[45,71,46],[45,72,27],[45,72,28],[45,72,29],[45,72,30],[45,72,37],[45,72,38],[45,72,39],[45,72,40],[45,72,41],[45,72,42],[45,72,43],[45,72,45],[45,72,46],[45,73,37],[45,73,38],[45,73,39],[45,73,40],[45,73,41],[45,73,42],[45,74,34],[45,74,35],[45,74,36],[45,74,37],[45,74,38],[45,74,39],[45,74,40],[45,74,41],[45,75,30],[45,75,31],[45,75,32],[45,75,34],[45,75,35],[45,75,36],[45,75,37],[45,75,38],[45,75,39],[45,75,40],[45,76,29],[45,76,30],[45,76,31],[45,76,32],[45,76,34],[45,76,35],[45,76,36],[45,76,37],[45,76,38],[45,77,29],[45,77,30],[45,77,31],[45,77,32],[45,78,29],[45,78,30],[45,78,31],[45,78,32],[46,57,50],[46,57,51],[46,57,52],[46,57,53],[46,58,50],[46,58,51],[46,58,52],[46,58,53],[46,59,48],[46,59,49],[46,59,50],[46,59,51],[46,59,52],[46,59,53],[46,59,54],[46,59,55],[46,59,56],[46,60,48],[46,60,49],[46,60,50],[46,60,53],[46,60,54],[46,60,55],[46,60,56],[46,61,48],[46,61,49],[46,61,50],[46,61,53],[46,61,54],[46,61,55],[46,61,56],[46,62,45],[46,62,46],[46,62,47],[46,62,48],[46,62,49],[46,62,50],[46,63,45],[46,63,46],[46,63,47],[46,63,48],[46,63,51],[46,63,52],[46,63,53],[46,64,45],[46,64,46],[46,64,47],[46,64,48],[46,64,50],[46,64,51],[46,64,52],[46,64,53],[46,65,43],[46,65,44],[46,65,45],[46,65,46],[46,65,47],[46,65,48],[46,65,49],[46,65,50],[46,65,51],[46,65,52],[46,65,53],[46,66,43],[46,66,44],[46,66,45],[46,66,46],[46,66,47],[46,66,48],[46,66,49],[46,66,50],[46,66,51],[46,66,52],[46,66,53],[46,67,43],[46,67,44],[46,67,45],[46,67,46],[46,67,47],[46,67,48],[46,67,49],[46,68,45],[46,68,46],[46,68,47],[46,69,45],[46,69,46],[46,69,47],[46,70,27],[46,70,28],[46,70,29],[46,70,30],[46,70,41],[46,70,45],[46,70,46],[46,71,27],[46,71,28],[46,71,29],[46,71,30],[46,71,39],[46,71,40],[46,71,41],[46,71,42],[46,71,43],[46,71,44],[46,71,45],[46,72,27],[46,72,28],[46,72,29],[46,72,30],[46,72,37],[46,72,38],[46,72,39],[46,72,40],[46,72,41],[46,72,42],[46,72,43],[46,72,44],[46,73,37],[46,73,38],[46,73,39],[46,73,40],[46,73,41],[46,73,42],[46,74,34],[46,74,35],[46,74,36],[46,74,37],[46,74,38],[46,74,39],[46,74,40],[46,75,29],[46,75,32],[46,75,34],[46,75,35],[46,75,36],[46,75,37],[46,75,38],[46,76,29],[46,76,30],[46,76,31],[46,76,32],[46,76,34],[46,76,35],[46,76,36],[46,76,37],[46,76,38],[46,77,29],[46,77,30],[46,77,31],[46,77,32],[46,78,29],[46,78,30],[46,78,31],[46,78,32],[46,79,30],[46,79,31],[47,57,50],[47,57,51],[47,57,52],[47,57,53],[47,58,50],[47,58,51],[47,58,52],[47,58,53],[47,59,48],[47,59,49],[47,59,50],[47,59,51],[47,59,52],[47,59,53],[47,59,54],[47,59,55],[47,59,56],[47,60,48],[47,60,49],[47,60,50],[47,60,53],[47,60,54],[47,60,55],[47,60,56],[47,61,49],[47,61,50],[47,61,53],[47,61,54],[47,61,55],[47,61,56],[47,62,47],[47,62,48],[47,62,49],[47,62,50],[47,63,47],[47,63,48],[47,64,47],[47,64,48],[47,65,47],[47,65,48],[47,65,49],[47,65,50],[47,66,46],[47,66,47],[47,66,48],[47,66,49],[47,66,50],[47,66,51],[47,67,45],[47,67,46],[47,67,47],[47,67,48],[47,67,49],[47,67,50],[47,67,51],[47,68,45],[47,68,46],[47,68,47],[47,69,41],[47,69,42],[47,69,43],[47,69,45],[47,69,46],[47,69,47],[47,70,41],[47,70,42],[47,70,43],[47,70,44],[47,70,45],[47,71,39],[47,71,40],[47,71,41],[47,71,42],[47,71,43],[47,71,44],[47,72,37],[47,72,38],[47,72,39],[47,72,40],[47,72,41],[47,72,42],[47,72,43],[47,72,44],[47,72,45],[47,73,37],[47,73,38],[47,73,39],[47,73,40],[47,74,34],[47,74,35],[47,74,36],[47,74,37],[47,74,38],[47,74,39],[47,74,40],[47,75,29],[47,75,32],[47,75,34],[47,75,35],[47,75,36],[47,75,37],[47,75,38],[47,76,29],[47,76,30],[47,76,31],[47,76,32],[47,76,34],[47,76,35],[47,76,36],[47,76,37],[47,76,38],[47,77,29],[47,77,30],[47,77,31],[47,77,32],[47,77,36],[47,77,37],[47,77,38],[47,78,30],[47,78,31],[48,57,50],[48,57,51],[48,57,52],[48,58,53],[48,58,54],[48,58,55],[48,58,56],[48,59,48],[48,59,49],[48,59,50],[48,59,53],[48,59,54],[48,59,55],[48,59,56],[48,60,48],[48,60,49],[48,60,50],[48,60,53],[48,60,54],[48,60,55],[48,60,56],[48,61,48],[48,61,49],[48,61,50],[48,61,53],[48,61,54],[48,61,55],[48,62,48],[48,62,49],[48,62,50],[48,63,48],[48,64,47],[48,64,48],[48,65,48],[48,65,49],[48,65,50],[48,66,47],[48,66,48],[48,66,49],[48,66,50],[48,66,51],[48,67,47],[48,67,48],[48,67,49],[48,67,50],[48,67,51],[48,68,45],[48,68,46],[48,69,45],[48,69,46],[48,70,41],[48,70,42],[48,70,43],[48,70,44],[48,70,45],[48,70,46],[48,71,39],[48,71,40],[48,71,41],[48,71,42],[48,71,43],[48,71,44],[48,71,45],[48,72,38],[48,72,39],[48,72,40],[48,72,41],[48,72,42],[48,72,43],[48,72,44],[48,72,45],[48,73,37],[48,73,38],[48,73,39],[48,73,40],[48,73,41],[48,74,34],[48,74,35],[48,74,36],[48,74,37],[48,74,38],[48,74,39],[48,74,40],[48,75,29],[48,75,30],[48,75,31],[48,75,32],[48,75,34],[48,75,35],[48,75,36],[48,75,37],[48,75,38],[48,76,29],[48,76,30],[48,76,31],[48,76,34],[48,76,35],[48,76,36],[48,76,37],[48,76,38],[48,77,30],[48,77,31],[48,77,36],[48,77,37],[48,77,38],[49,58,53],[49,58,54],[49,58,55],[49,58,56],[49,59,53],[49,59,54],[49,59,55],[49,59,56],[49,60,53],[49,60,54],[49,60,55],[49,60,56],[49,70,41],[49,70,42],[49,70,43],[49,70,44],[49,70,45],[49,71,41],[49,71,42],[49,71,43],[49,71,44],[49,71,45],[49,72,39],[49,72,40],[49,72,41],[49,72,42],[49,72,43],[49,72,44],[49,72,45],[49,73,37],[49,73,38],[49,73,39],[49,73,40],[49,73,41],[49,74,38],[49,74,39],[49,74,40],[49,75,36],[49,75,37],[49,75,38],[49,76,36],[49,76,37],[49,76,38],[49,77,36],[49,77,37],[49,77,38],[50,58,53],[50,58,54],[50,58,55],[50,59,53],[50,59,54],[50,59,55],[50,60,53],[50,60,54],[50,60,55],[50,72,39],[50,72,40],[50,72,41],[50,72,43],[50,73,38],[50,73,39],[50,73,40],[50,73,41],[50,74,38],[50,74,39],[50,74,40]],"ignoreExtent":false,"flags":4096},"109":{"id":109,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,68,43],[39,69,44]],"colors":[[1,0,0,1]],"centers":[[38,68,43],[39,69,44]],"ignoreExtent":false,"flags":16448},"110":{"id":110,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,72,40],[32,71,39]],"colors":[[1,0,0,1]],"centers":[[31,72,40],[32,71,39]],"ignoreExtent":false,"flags":16448},"111":{"id":111,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,44],[37,66,45]],"colors":[[1,0,0,1]],"centers":[[36,67,44],[37,66,45]],"ignoreExtent":false,"flags":16448},"112":{"id":112,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,41],[37,73,42]],"colors":[[1,0,0,1]],"centers":[[36,74,41],[37,73,42]],"ignoreExtent":false,"flags":16448},"113":{"id":113,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,70,46],[39,71,45]],"colors":[[1,0,0,1]],"centers":[[38,70,46],[39,71,45]],"ignoreExtent":false,"flags":16448},"114":{"id":114,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[35,66,44]],"colors":[[1,0,0,1]],"centers":[[34,67,45],[35,66,44]],"ignoreExtent":false,"flags":16448},"115":{"id":115,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,40],[32,70,41]],"colors":[[1,0,0,1]],"centers":[[31,71,40],[32,70,41]],"ignoreExtent":false,"flags":16448},"116":{"id":116,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,38],[34,71,39]],"colors":[[1,0,0,1]],"centers":[[33,72,38],[34,71,39]],"ignoreExtent":false,"flags":16448},"117":{"id":117,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,44],[33,69,45]],"colors":[[1,0,0,1]],"centers":[[32,70,44],[33,69,45]],"ignoreExtent":false,"flags":16448},"118":{"id":118,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,75,39]],"colors":[[1,0,0,1]],"centers":[[36,74,40],[36,75,39]],"ignoreExtent":false,"flags":16448},"119":{"id":119,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,38]],"colors":[[1,0,0,1]],"centers":[[34,71,39],[35,71,38]],"ignoreExtent":false,"flags":16448},"120":{"id":120,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,45],[39,70,44]],"colors":[[1,0,0,1]],"centers":[[39,69,45],[39,70,44]],"ignoreExtent":false,"flags":16448},"121":{"id":121,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,40],[31,72,39]],"colors":[[1,0,0,1]],"centers":[[31,71,40],[31,72,39]],"ignoreExtent":false,"flags":16448},"122":{"id":122,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,74,36],[35,75,37]],"colors":[[1,0,0,1]],"centers":[[35,74,36],[35,75,37]],"ignoreExtent":false,"flags":16448},"123":{"id":123,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,65,44],[36,66,43]],"colors":[[1,0,0,1]],"centers":[[36,65,44],[36,66,43]],"ignoreExtent":false,"flags":16448},"124":{"id":124,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,67,43],[36,66,43]],"colors":[[1,0,0,1]],"centers":[[35,67,43],[36,66,43]],"ignoreExtent":false,"flags":16448},"125":{"id":125,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[36,68,43]],"colors":[[1,0,0,1]],"centers":[[36,67,43],[36,68,43]],"ignoreExtent":false,"flags":16448},"126":{"id":126,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,70,44],[39,71,45]],"colors":[[1,0,0,1]],"centers":[[39,70,44],[39,71,45]],"ignoreExtent":false,"flags":16448},"127":{"id":127,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,65,45],[37,66,45]],"colors":[[1,0,0,1]],"centers":[[36,65,45],[37,66,45]],"ignoreExtent":false,"flags":16448},"128":{"id":128,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,39],[31,72,39]],"colors":[[1,0,0,1]],"centers":[[31,71,39],[31,72,39]],"ignoreExtent":false,"flags":16448},"129":{"id":129,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,66,44],[35,67,43]],"colors":[[1,0,0,1]],"centers":[[35,66,44],[35,67,43]],"ignoreExtent":false,"flags":16448},"130":{"id":130,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[37,71,45]],"colors":[[1,0,0,1]],"centers":[[37,70,46],[37,71,45]],"ignoreExtent":false,"flags":16448},"131":{"id":131,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,44],[33,70,43]],"colors":[[1,0,0,1]],"centers":[[32,70,44],[33,70,43]],"ignoreExtent":false,"flags":16448},"132":{"id":132,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,40],[33,72,40]],"colors":[[1,0,0,1]],"centers":[[32,71,40],[33,72,40]],"ignoreExtent":false,"flags":16448},"133":{"id":133,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,66,43],[37,67,43]],"colors":[[1,0,0,1]],"centers":[[36,66,43],[37,67,43]],"ignoreExtent":false,"flags":16448},"134":{"id":134,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[33,72,40]],"colors":[[1,0,0,1]],"centers":[[33,71,39],[33,72,40]],"ignoreExtent":false,"flags":16448},"135":{"id":135,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,68,43],[37,68,43]],"colors":[[1,0,0,1]],"centers":[[36,68,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"136":{"id":136,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,43],[34,70,42]],"colors":[[1,0,0,1]],"centers":[[33,70,43],[34,70,42]],"ignoreExtent":false,"flags":16448},"137":{"id":137,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,72,44],[37,73,43]],"colors":[[1,0,0,1]],"centers":[[37,72,44],[37,73,43]],"ignoreExtent":false,"flags":16448},"138":{"id":138,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,40],[31,72,41]],"colors":[[1,0,0,1]],"centers":[[31,71,40],[31,72,41]],"ignoreExtent":false,"flags":16448},"139":{"id":139,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[37,68,43]],"colors":[[1,0,0,1]],"centers":[[36,67,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"140":{"id":140,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[35,74,36]],"colors":[[1,0,0,1]],"centers":[[34,74,37],[35,74,36]],"ignoreExtent":false,"flags":16448},"141":{"id":141,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[37,72,44]],"colors":[[1,0,0,1]],"centers":[[37,71,45],[37,72,44]],"ignoreExtent":false,"flags":16448},"142":{"id":142,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,74,37]],"colors":[[1,0,0,1]],"centers":[[33,73,37],[34,74,37]],"ignoreExtent":false,"flags":16448},"143":{"id":143,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[34,67,45]],"colors":[[1,0,0,1]],"centers":[[33,67,45],[34,67,45]],"ignoreExtent":false,"flags":16448},"144":{"id":144,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,37],[35,75,38]],"colors":[[1,0,0,1]],"centers":[[35,75,37],[35,75,38]],"ignoreExtent":false,"flags":16448},"145":{"id":145,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,43],[37,68,43]],"colors":[[1,0,0,1]],"centers":[[37,67,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"146":{"id":146,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[36,67,44]],"colors":[[1,0,0,1]],"centers":[[36,67,43],[36,67,44]],"ignoreExtent":false,"flags":16448},"147":{"id":147,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,38],[35,75,39]],"colors":[[1,0,0,1]],"centers":[[35,75,38],[35,75,39]],"ignoreExtent":false,"flags":16448},"148":{"id":148,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,39],[32,71,40]],"colors":[[1,0,0,1]],"centers":[[32,71,39],[32,71,40]],"ignoreExtent":false,"flags":16448},"149":{"id":149,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[38,70,46]],"colors":[[1,0,0,1]],"centers":[[37,70,46],[38,70,46]],"ignoreExtent":false,"flags":16448},"150":{"id":150,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,41],[34,70,41]],"colors":[[1,0,0,1]],"centers":[[33,70,41],[34,70,41]],"ignoreExtent":false,"flags":16448},"151":{"id":151,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[33,73,37]],"colors":[[1,0,0,1]],"centers":[[33,72,37],[33,73,37]],"ignoreExtent":false,"flags":16448},"152":{"id":152,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,66,43],[36,67,43]],"colors":[[1,0,0,1]],"centers":[[36,66,43],[36,67,43]],"ignoreExtent":false,"flags":16448},"153":{"id":153,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,71,38],[35,71,39]],"colors":[[1,0,0,1]],"centers":[[35,71,38],[35,71,39]],"ignoreExtent":false,"flags":16448},"154":{"id":154,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,39],[31,71,40]],"colors":[[1,0,0,1]],"centers":[[31,71,39],[31,71,40]],"ignoreExtent":false,"flags":16448},"155":{"id":155,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[33,68,45]],"colors":[[1,0,0,1]],"centers":[[33,67,45],[33,68,45]],"ignoreExtent":false,"flags":16448},"156":{"id":156,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,39]],"colors":[[1,0,0,1]],"centers":[[34,71,39],[35,71,39]],"ignoreExtent":false,"flags":16448},"157":{"id":157,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,39],[36,75,39]],"colors":[[1,0,0,1]],"centers":[[35,75,39],[36,75,39]],"ignoreExtent":false,"flags":16448},"158":{"id":158,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[33,69,45]],"colors":[[1,0,0,1]],"centers":[[33,68,45],[33,69,45]],"ignoreExtent":false,"flags":16448},"159":{"id":159,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,41],[34,70,42]],"colors":[[1,0,0,1]],"centers":[[34,70,41],[34,70,42]],"ignoreExtent":false,"flags":16448},"160":{"id":160,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[38,68,43]],"colors":[[1,0,0,1]],"centers":[[37,68,43],[38,68,43]],"ignoreExtent":false,"flags":16448},"161":{"id":161,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,74,41]],"colors":[[1,0,0,1]],"centers":[[36,74,40],[36,74,41]],"ignoreExtent":false,"flags":16448},"162":{"id":162,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[33,72,38]],"colors":[[1,0,0,1]],"centers":[[33,72,37],[33,72,38]],"ignoreExtent":false,"flags":16448},"163":{"id":163,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,41],[33,70,41]],"colors":[[1,0,0,1]],"centers":[[32,70,41],[33,70,41]],"ignoreExtent":false,"flags":16448},"164":{"id":164,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,44],[39,69,45]],"colors":[[1,0,0,1]],"centers":[[39,69,44],[39,69,45]],"ignoreExtent":false,"flags":16448},"165":{"id":165,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,65,44],[36,65,45]],"colors":[[1,0,0,1]],"centers":[[36,65,44],[36,65,45]],"ignoreExtent":false,"flags":16448},"166":{"id":166,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[34,71,39]],"colors":[[1,0,0,1]],"centers":[[33,71,39],[34,71,39]],"ignoreExtent":false,"flags":16448},"167":{"id":167,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,73,43]],"colors":[[1,0,0,1]],"centers":[[37,73,42],[37,73,43]],"ignoreExtent":false,"flags":16448},"168":{"id":168,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,72,40],[31,72,41]],"colors":[[1,0,0,1]],"centers":[[31,72,40],[31,72,41]],"ignoreExtent":false,"flags":16448},"106":{"id":106,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"105":{"id":105,"type":"background","material":{"fog":true},"colors":[[0.2980392,0.2980392,0.2980392,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"107":{"id":107,"type":"background","material":{"lit":false,"back":"lines"},"colors":[[1,1,1,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"102":{"id":102,"type":"subscene","par3d":{"antialias":0,"FOV":30,"ignoreExtent":false,"listeners":102,"mouseMode":{"left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,80.23582],"modelMatrix":[[0.3630731,-0.5154495,0.7762021,-11.68474],[0.7040736,0.6974096,0.1337916,-81.13914],[-0.6102936,0.4979273,0.616125,-115.2523],[0,0,0,1]],"projMatrix":[[2.665751,0,0,0],[0,3.732051,0,0],[0,0,-3.863704,-289.2409],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[0.3630731,-0.5154495,0.7762021,0],[0.7040736,0.6974096,0.1337916,0],[-0.6102936,0.4979273,0.616125,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[30,50,57,79,27,56],"windowRect":[100,128,772,608],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"/home/dyslexicon/R/x86_64-pc-linux-gnu-library/3.2/rgl/fonts/FreeSans.ttf","maxClipPlanes":8,"glVersion":3},"embeddings":{"viewport":"replace","projection":"replace","model":"replace"},"objects":[107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,106],"subscenes":[],"flags":21056}},"snapshot":"data:image/png;base64,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","width":673,"height":481,"sphereVerts":{"reuse":"unnamed_chunk_3div"},"context":{"shiny":false,"rmarkdown":"github_document"},"crosstalk":{"key":[],"group":[],"id":[],"options":[]}});
unnamed_chunk_5rgl.prefix = "unnamed_chunk_5";
</script>
<p id="unnamed_chunk_5debug">
You must enable Javascript to view this page properly.
</p>
<script>unnamed_chunk_5rgl.start();</script>
If we see, as with this grape, that it is likely part of a stable vine we can find the vine that spans all time slices. Here is a look at the first six in this vine.

``` r
loops<-vinehunter_primitive(4775,diags = data[1:6])

plot_loops(loops,data = acc)
```

<script type="text/javascript">
var unnamed_chunk_6div = document.getElementById("unnamed_chunk_6div"),
unnamed_chunk_6rgl = new rglwidgetClass();
unnamed_chunk_6div.width = 673;
unnamed_chunk_6div.height = 481;
unnamed_chunk_6rgl.initialize(unnamed_chunk_6div,
{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":false,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false},"rootSubscene":169,"objects":{"175":{"id":175,"type":"points","material":{"lit":false},"vertices":[[30,72,41],[30,73,38],[30,73,39],[31,58,53],[31,58,54],[31,58,55],[31,58,56],[31,59,53],[31,59,54],[31,59,55],[31,59,56],[31,60,53],[31,60,54],[31,60,55],[31,60,56],[31,70,41],[31,70,42],[31,70,43],[31,70,44],[31,71,39],[31,71,40],[31,71,41],[31,71,42],[31,71,43],[31,71,44],[31,72,39],[31,72,40],[31,72,41],[31,72,42],[31,72,43],[31,72,44],[31,72,45],[31,73,37],[31,73,38],[31,73,39],[31,73,40],[31,73,41],[31,73,42],[31,74,38],[31,74,39],[31,74,40],[31,75,36],[31,75,37],[31,75,38],[31,76,36],[31,76,37],[31,76,38],[31,77,36],[31,77,37],[31,77,38],[32,57,50],[32,57,51],[32,57,52],[32,57,53],[32,58,51],[32,58,53],[32,58,54],[32,58,55],[32,58,56],[32,59,53],[32,59,54],[32,59,55],[32,59,56],[32,60,48],[32,60,49],[32,60,50],[32,60,53],[32,60,54],[32,60,55],[32,60,56],[32,61,48],[32,61,49],[32,61,50],[32,62,48],[32,62,49],[32,62,50],[32,65,48],[32,65,49],[32,65,50],[32,66,47],[32,66,48],[32,66,49],[32,66,50],[32,66,51],[32,67,47],[32,67,48],[32,67,49],[32,67,50],[32,67,51],[32,70,41],[32,70,42],[32,70,43],[32,70,44],[32,71,39],[32,71,40],[32,71,41],[32,71,42],[32,71,43],[32,71,44],[32,72,38],[32,72,39],[32,72,40],[32,72,41],[32,72,42],[32,72,43],[32,72,44],[32,72,45],[32,73,37],[32,73,38],[32,73,39],[32,73,40],[32,73,41],[32,74,37],[32,74,38],[32,74,39],[32,74,40],[32,74,41],[32,75,29],[32,75,30],[32,75,31],[32,75,32],[32,75,36],[32,76,29],[32,76,30],[32,76,31],[32,76,34],[32,76,35],[32,76,36],[32,77,30],[32,77,31],[33,57,50],[33,57,51],[33,57,52],[33,57,53],[33,58,50],[33,58,51],[33,58,52],[33,58,53],[33,59,48],[33,59,49],[33,59,50],[33,59,51],[33,59,52],[33,59,53],[33,59,54],[33,59,55],[33,59,56],[33,60,48],[33,60,49],[33,60,50],[33,60,53],[33,60,54],[33,60,55],[33,60,56],[33,61,48],[33,61,49],[33,61,50],[33,61,53],[33,61,54],[33,61,55],[33,61,56],[33,62,47],[33,62,48],[33,62,49],[33,62,50],[33,63,47],[33,63,48],[33,64,47],[33,64,48],[33,65,48],[33,65,49],[33,65,50],[33,66,47],[33,66,48],[33,66,49],[33,66,50],[33,66,51],[33,67,45],[33,67,46],[33,67,47],[33,67,48],[33,67,49],[33,67,50],[33,67,51],[33,68,45],[33,68,46],[33,68,47],[33,69,45],[33,69,46],[33,69,47],[33,70,41],[33,70,42],[33,70,43],[33,70,44],[33,70,45],[33,70,46],[33,71,39],[33,71,40],[33,71,41],[33,71,42],[33,71,43],[33,71,44],[33,72,37],[33,72,38],[33,72,39],[33,72,40],[33,72,41],[33,72,42],[33,72,43],[33,72,44],[33,72,45],[33,73,37],[33,73,38],[33,73,39],[33,73,40],[33,74,34],[33,74,35],[33,74,36],[33,74,37],[33,74,38],[33,74,39],[33,74,40],[33,75,29],[33,75,30],[33,75,31],[33,75,32],[33,75,34],[33,75,35],[33,75,36],[33,75,37],[33,75,38],[33,76,29],[33,76,30],[33,76,31],[33,76,32],[33,76,34],[33,76,35],[33,76,36],[33,76,37],[33,76,38],[33,77,29],[33,77,30],[33,77,31],[33,77,32],[33,78,29],[33,78,30],[33,78,31],[33,78,32],[34,57,50],[34,57,51],[34,57,52],[34,57,53],[34,58,50],[34,58,51],[34,58,52],[34,58,53],[34,59,48],[34,59,49],[34,59,50],[34,59,51],[34,59,52],[34,59,53],[34,59,54],[34,59,55],[34,59,56],[34,60,48],[34,60,49],[34,60,50],[34,60,53],[34,60,54],[34,60,55],[34,60,56],[34,61,48],[34,61,49],[34,61,50],[34,61,53],[34,61,54],[34,61,55],[34,61,56],[34,62,47],[34,62,48],[34,62,49],[34,62,50],[34,63,47],[34,63,48],[34,64,47],[34,64,48],[34,65,46],[34,65,47],[34,65,48],[34,65,49],[34,65,50],[34,65,51],[34,65,52],[34,65,53],[34,66,46],[34,66,47],[34,66,48],[34,66,49],[34,66,50],[34,66,51],[34,66,52],[34,67,45],[34,67,46],[34,67,47],[34,67,48],[34,67,49],[34,67,50],[34,67,51],[34,68,45],[34,68,46],[34,69,45],[34,69,46],[34,70,41],[34,70,42],[34,70,43],[34,70,44],[34,70,45],[34,70,46],[34,71,39],[34,71,40],[34,71,41],[34,71,42],[34,71,43],[34,71,44],[34,71,45],[34,72,37],[34,72,38],[34,72,39],[34,72,40],[34,72,41],[34,72,42],[34,72,43],[34,72,44],[34,72,45],[34,73,37],[34,73,38],[34,73,39],[34,73,40],[34,74,34],[34,74,35],[34,74,36],[34,74,37],[34,74,40],[34,75,29],[34,75,32],[34,75,34],[34,75,35],[34,75,36],[34,75,37],[34,75,38],[34,76,29],[34,76,30],[34,76,31],[34,76,32],[34,76,34],[34,76,35],[34,76,36],[34,76,37],[34,76,38],[34,77,29],[34,77,30],[34,77,31],[34,77,32],[34,78,29],[34,78,30],[34,78,31],[34,78,32],[34,79,30],[34,79,31],[35,57,50],[35,57,51],[35,57,52],[35,57,53],[35,58,50],[35,58,51],[35,58,52],[35,58,53],[35,59,48],[35,59,49],[35,59,50],[35,59,51],[35,59,52],[35,59,53],[35,59,54],[35,59,55],[35,59,56],[35,60,48],[35,60,49],[35,60,50],[35,60,53],[35,60,54],[35,60,55],[35,60,56],[35,61,48],[35,61,49],[35,61,50],[35,61,53],[35,61,54],[35,61,55],[35,61,56],[35,62,46],[35,62,47],[35,62,48],[35,62,49],[35,62,50],[35,63,45],[35,63,46],[35,63,47],[35,63,48],[35,63,50],[35,63,51],[35,63,52],[35,63,53],[35,64,45],[35,64,46],[35,64,47],[35,64,48],[35,64,50],[35,64,51],[35,64,52],[35,64,53],[35,65,43],[35,65,44],[35,65,45],[35,65,46],[35,65,47],[35,65,48],[35,65,49],[35,65,50],[35,65,51],[35,65,52],[35,65,53],[35,66,43],[35,66,44],[35,66,47],[35,66,48],[35,66,49],[35,66,50],[35,66,51],[35,66,52],[35,67,43],[35,67,44],[35,67,45],[35,67,46],[35,67,47],[35,67,48],[35,68,43],[35,68,44],[35,68,45],[35,68,46],[35,69,45],[35,69,46],[35,69,47],[35,70,27],[35,70,28],[35,70,29],[35,70,30],[35,70,41],[35,70,45],[35,70,46],[35,70,47],[35,71,27],[35,71,28],[35,71,29],[35,71,30],[35,71,38],[35,71,39],[35,71,40],[35,71,41],[35,71,42],[35,71,43],[35,71,44],[35,71,45],[35,71,46],[35,72,27],[35,72,28],[35,72,29],[35,72,30],[35,72,37],[35,72,38],[35,72,39],[35,72,40],[35,72,41],[35,72,42],[35,72,43],[35,73,37],[35,73,38],[35,73,39],[35,73,40],[35,73,41],[35,73,42],[35,74,32],[35,74,33],[35,74,34],[35,74,35],[35,74,36],[35,74,37],[35,74,38],[35,74,39],[35,74,40],[35,74,41],[35,75,30],[35,75,31],[35,75,32],[35,75,33],[35,75,34],[35,75,35],[35,75,36],[35,75,37],[35,75,38],[35,75,39],[35,76,29],[35,76,30],[35,76,31],[35,76,32],[35,76,33],[35,76,34],[35,76,35],[35,76,36],[35,76,37],[35,76,38],[35,77,29],[35,77,30],[35,77,31],[35,77,32],[35,77,34],[35,78,29],[35,78,30],[35,78,31],[35,78,32],[35,79,30],[35,79,31],[36,57,50],[36,57,51],[36,57,52],[36,57,53],[36,58,50],[36,58,51],[36,58,52],[36,58,53],[36,59,49],[36,59,50],[36,59,51],[36,59,52],[36,59,53],[36,59,54],[36,59,55],[36,59,56],[36,60,48],[36,60,49],[36,60,50],[36,60,53],[36,60,54],[36,60,55],[36,60,56],[36,61,47],[36,61,48],[36,61,49],[36,61,50],[36,61,53],[36,61,54],[36,61,55],[36,61,56],[36,62,47],[36,62,48],[36,62,49],[36,62,50],[36,63,45],[36,63,46],[36,63,47],[36,63,48],[36,63,50],[36,63,51],[36,63,52],[36,63,53],[36,64,45],[36,64,46],[36,64,47],[36,64,48],[36,64,50],[36,64,51],[36,64,52],[36,64,53],[36,65,43],[36,65,44],[36,65,45],[36,65,46],[36,65,47],[36,65,48],[36,65,49],[36,65,50],[36,65,51],[36,65,52],[36,65,53],[36,66,43],[36,66,44],[36,66,45],[36,66,46],[36,66,47],[36,66,48],[36,66,49],[36,66,50],[36,66,51],[36,66,52],[36,66,53],[36,67,43],[36,67,44],[36,67,45],[36,67,46],[36,67,47],[36,67,48],[36,68,43],[36,68,44],[36,68,45],[36,69,45],[36,69,46],[36,70,39],[36,70,40],[36,70,41],[36,70,42],[36,70,43],[36,70,45],[36,70,46],[36,71,27],[36,71,28],[36,71,37],[36,71,38],[36,71,39],[36,71,40],[36,71,41],[36,71,42],[36,71,43],[36,71,44],[36,71,45],[36,71,46],[36,72,27],[36,72,28],[36,72,29],[36,72,30],[36,72,37],[36,72,39],[36,72,40],[36,72,41],[36,72,42],[36,72,43],[36,72,45],[36,72,46],[36,73,37],[36,73,38],[36,73,39],[36,73,40],[36,73,41],[36,73,42],[36,73,43],[36,74,32],[36,74,33],[36,74,34],[36,74,35],[36,74,36],[36,74,37],[36,74,38],[36,74,39],[36,74,40],[36,74,41],[36,75,30],[36,75,31],[36,75,32],[36,75,33],[36,75,34],[36,75,35],[36,75,36],[36,75,37],[36,75,38],[36,75,39],[36,75,40],[36,76,29],[36,76,30],[36,76,31],[36,76,32],[36,76,33],[36,76,34],[36,76,35],[36,76,36],[36,76,37],[36,76,38],[36,76,39],[36,77,29],[36,77,30],[36,77,31],[36,77,32],[36,77,33],[36,77,34],[36,78,29],[36,78,30],[36,78,31],[36,78,32],[37,57,50],[37,57,51],[37,57,52],[37,57,53],[37,57,54],[37,57,55],[37,58,50],[37,58,51],[37,58,52],[37,58,53],[37,58,54],[37,58,55],[37,58,56],[37,59,49],[37,59,50],[37,59,51],[37,59,52],[37,59,53],[37,59,54],[37,59,55],[37,59,56],[37,60,48],[37,60,49],[37,60,50],[37,60,51],[37,60,52],[37,60,53],[37,60,54],[37,60,55],[37,60,56],[37,61,47],[37,61,48],[37,61,49],[37,61,50],[37,61,51],[37,61,52],[37,61,53],[37,62,46],[37,62,47],[37,62,48],[37,62,49],[37,62,50],[37,62,51],[37,62,52],[37,62,53],[37,63,45],[37,63,46],[37,63,47],[37,63,48],[37,63,50],[37,63,51],[37,63,52],[37,63,53],[37,64,27],[37,64,28],[37,64,29],[37,64,45],[37,64,46],[37,64,47],[37,64,48],[37,64,49],[37,64,50],[37,64,51],[37,64,52],[37,64,53],[37,65,27],[37,65,28],[37,65,29],[37,65,45],[37,65,46],[37,65,47],[37,65,48],[37,65,49],[37,65,50],[37,65,51],[37,65,52],[37,65,53],[37,66,27],[37,66,28],[37,66,29],[37,66,43],[37,66,44],[37,66,45],[37,66,46],[37,66,47],[37,66,48],[37,66,49],[37,66,50],[37,66,51],[37,66,52],[37,66,53],[37,67,27],[37,67,28],[37,67,29],[37,67,43],[37,67,44],[37,67,45],[37,67,46],[37,67,47],[37,67,48],[37,67,49],[37,67,50],[37,67,51],[37,68,27],[37,68,28],[37,68,29],[37,68,43],[37,68,44],[37,68,45],[37,68,47],[37,68,48],[37,69,27],[37,69,28],[37,69,29],[37,69,30],[37,69,41],[37,69,42],[37,69,43],[37,69,44],[37,69,45],[37,70,27],[37,70,28],[37,70,29],[37,70,30],[37,70,31],[37,70,39],[37,70,40],[37,70,41],[37,70,42],[37,70,43],[37,70,44],[37,70,45],[37,70,46],[37,71,27],[37,71,28],[37,71,29],[37,71,30],[37,71,31],[37,71,32],[37,71,37],[37,71,38],[37,71,39],[37,71,40],[37,71,41],[37,71,42],[37,71,43],[37,71,44],[37,71,45],[37,71,46],[37,72,27],[37,72,28],[37,72,29],[37,72,30],[37,72,31],[37,72,32],[37,72,37],[37,72,38],[37,72,39],[37,72,40],[37,72,41],[37,72,42],[37,72,43],[37,72,44],[37,72,45],[37,72,46],[37,73,27],[37,73,28],[37,73,29],[37,73,30],[37,73,31],[37,73,32],[37,73,33],[37,73,34],[37,73,35],[37,73,36],[37,73,37],[37,73,38],[37,73,39],[37,73,40],[37,73,41],[37,73,42],[37,73,43],[37,73,44],[37,73,45],[37,74,27],[37,74,28],[37,74,29],[37,74,30],[37,74,31],[37,74,32],[37,74,33],[37,74,34],[37,74,35],[37,74,36],[37,74,37],[37,74,38],[37,74,39],[37,74,40],[37,74,41],[37,74,42],[37,74,43],[37,74,44],[37,75,29],[37,75,30],[37,75,31],[37,75,32],[37,75,33],[37,75,34],[37,75,35],[37,75,36],[37,75,37],[37,75,38],[37,75,39],[37,75,40],[37,76,29],[37,76,30],[37,76,31],[37,76,32],[37,76,33],[37,76,34],[37,76,35],[37,76,36],[37,76,37],[37,76,38],[37,76,39],[37,76,40],[37,77,29],[37,77,30],[37,77,31],[37,77,32],[37,77,33],[37,77,34],[37,77,35],[37,77,36],[37,77,37],[37,77,38],[37,78,29],[37,78,30],[37,78,31],[37,78,32],[37,78,33],[37,78,34],[37,78,35],[37,78,36],[37,78,37],[37,78,38],[37,79,31],[38,57,50],[38,57,51],[38,57,52],[38,57,53],[38,57,54],[38,57,55],[38,58,50],[38,58,51],[38,58,52],[38,58,53],[38,58,54],[38,58,55],[38,58,56],[38,59,50],[38,59,51],[38,59,52],[38,59,53],[38,59,54],[38,59,55],[38,59,56],[38,60,48],[38,60,49],[38,60,50],[38,60,51],[38,60,52],[38,60,53],[38,60,54],[38,60,55],[38,60,56],[38,61,47],[38,61,48],[38,61,49],[38,61,50],[38,61,51],[38,61,52],[38,61,53],[38,62,46],[38,62,47],[38,62,48],[38,62,49],[38,62,50],[38,62,51],[38,62,52],[38,62,53],[38,63,45],[38,63,46],[38,63,47],[38,63,48],[38,63,49],[38,63,50],[38,63,51],[38,63,52],[38,63,53],[38,64,27],[38,64,28],[38,64,29],[38,64,45],[38,64,46],[38,64,47],[38,64,48],[38,64,49],[38,64,50],[38,64,51],[38,64,52],[38,64,53],[38,65,27],[38,65,28],[38,65,29],[38,65,45],[38,65,46],[38,65,47],[38,65,48],[38,65,49],[38,65,50],[38,65,51],[38,65,52],[38,65,53],[38,66,27],[38,66,45],[38,66,46],[38,66,47],[38,66,48],[38,66,49],[38,66,50],[38,66,51],[38,66,52],[38,66,53],[38,67,27],[38,67,28],[38,67,29],[38,67,43],[38,67,45],[38,67,46],[38,67,47],[38,67,48],[38,67,49],[38,67,50],[38,67,51],[38,68,27],[38,68,28],[38,68,29],[38,68,43],[38,68,44],[38,68,45],[38,68,46],[38,68,47],[38,68,48],[38,69,27],[38,69,28],[38,69,29],[38,69,30],[38,69,41],[38,69,42],[38,69,43],[38,69,44],[38,69,45],[38,69,46],[38,69,47],[38,70,27],[38,70,28],[38,70,29],[38,70,30],[38,70,31],[38,70,40],[38,70,41],[38,70,42],[38,70,43],[38,70,44],[38,70,45],[38,70,46],[38,70,47],[38,71,27],[38,71,28],[38,71,29],[38,71,30],[38,71,31],[38,71,32],[38,71,37],[38,71,38],[38,71,39],[38,71,40],[38,71,41],[38,71,42],[38,71,43],[38,71,44],[38,71,45],[38,71,46],[38,71,47],[38,72,27],[38,72,28],[38,72,29],[38,72,30],[38,72,31],[38,72,32],[38,72,37],[38,72,38],[38,72,39],[38,72,40],[38,72,41],[38,72,42],[38,72,43],[38,72,44],[38,72,45],[38,72,46],[38,73,27],[38,73,28],[38,73,29],[38,73,30],[38,73,31],[38,73,32],[38,73,33],[38,73,34],[38,73,35],[38,73,36],[38,73,37],[38,73,38],[38,73,39],[38,73,40],[38,73,41],[38,73,42],[38,73,43],[38,73,44],[38,73,45],[38,74,27],[38,74,28],[38,74,29],[38,74,30],[38,74,31],[38,74,32],[38,74,33],[38,74,34],[38,74,35],[38,74,36],[38,74,37],[38,74,38],[38,74,39],[38,74,40],[38,74,41],[38,74,42],[38,74,43],[38,75,29],[38,75,30],[38,75,31],[38,75,32],[38,75,33],[38,75,34],[38,75,35],[38,75,36],[38,75,37],[38,75,38],[38,75,39],[38,75,40],[38,76,29],[38,76,30],[38,76,31],[38,76,32],[38,76,33],[38,76,34],[38,76,35],[38,76,36],[38,76,37],[38,76,38],[38,76,39],[38,76,40],[38,77,29],[38,77,30],[38,77,31],[38,77,32],[38,77,33],[38,77,34],[38,77,35],[38,77,36],[38,77,37],[38,77,38],[38,78,29],[38,78,30],[38,78,31],[38,78,32],[38,78,33],[38,78,34],[38,78,35],[38,78,36],[38,78,37],[38,78,38],[38,79,31],[39,57,54],[39,57,55],[39,58,50],[39,58,51],[39,58,52],[39,58,53],[39,58,54],[39,58,55],[39,58,56],[39,59,50],[39,59,51],[39,59,52],[39,59,53],[39,59,54],[39,59,55],[39,59,56],[39,60,49],[39,60,50],[39,60,51],[39,60,52],[39,60,53],[39,60,54],[39,60,55],[39,60,56],[39,61,48],[39,61,49],[39,61,50],[39,61,51],[39,61,52],[39,61,53],[39,62,47],[39,62,48],[39,62,49],[39,62,50],[39,62,51],[39,62,52],[39,62,53],[39,63,47],[39,63,48],[39,63,49],[39,63,50],[39,63,51],[39,63,52],[39,63,53],[39,64,47],[39,64,48],[39,64,49],[39,64,50],[39,64,51],[39,64,52],[39,64,53],[39,65,27],[39,65,45],[39,65,46],[39,65,47],[39,65,48],[39,65,49],[39,65,50],[39,65,51],[39,65,52],[39,65,53],[39,66,27],[39,66,45],[39,66,46],[39,66,47],[39,66,48],[39,66,49],[39,66,50],[39,66,51],[39,66,52],[39,66,53],[39,67,27],[39,67,28],[39,67,29],[39,67,45],[39,67,46],[39,67,47],[39,67,48],[39,67,49],[39,67,50],[39,67,51],[39,68,27],[39,68,28],[39,68,29],[39,68,45],[39,68,46],[39,68,47],[39,68,48],[39,69,27],[39,69,28],[39,69,29],[39,69,30],[39,69,41],[39,69,42],[39,69,43],[39,69,44],[39,69,45],[39,69,46],[39,69,47],[39,70,27],[39,70,28],[39,70,29],[39,70,30],[39,70,41],[39,70,42],[39,70,43],[39,70,44],[39,70,45],[39,70,46],[39,70,47],[39,71,27],[39,71,28],[39,71,29],[39,71,30],[39,71,31],[39,71,32],[39,71,37],[39,71,38],[39,71,39],[39,71,40],[39,71,41],[39,71,42],[39,71,43],[39,71,44],[39,71,45],[39,71,46],[39,71,47],[39,72,27],[39,72,28],[39,72,29],[39,72,30],[39,72,31],[39,72,32],[39,72,37],[39,72,38],[39,72,39],[39,72,40],[39,72,41],[39,72,42],[39,72,43],[39,72,44],[39,72,45],[39,72,46],[39,73,27],[39,73,28],[39,73,29],[39,73,30],[39,73,31],[39,73,32],[39,73,33],[39,73,34],[39,73,35],[39,73,36],[39,73,37],[39,73,38],[39,73,39],[39,73,40],[39,73,41],[39,73,42],[39,73,43],[39,73,44],[39,74,27],[39,74,28],[39,74,29],[39,74,30],[39,74,31],[39,74,32],[39,74,33],[39,74,34],[39,74,35],[39,74,36],[39,74,37],[39,74,38],[39,74,39],[39,74,40],[39,74,41],[39,75,29],[39,75,30],[39,75,31],[39,75,32],[39,75,33],[39,75,34],[39,75,35],[39,75,36],[39,75,37],[39,75,38],[39,75,39],[39,75,40],[39,76,29],[39,76,30],[39,76,31],[39,76,32],[39,76,33],[39,76,34],[39,76,35],[39,76,36],[39,76,37],[39,76,38],[39,76,39],[39,77,29],[39,77,30],[39,77,31],[39,77,32],[39,77,33],[39,77,34],[39,77,35],[39,77,36],[39,77,37],[39,77,38],[39,78,31],[39,78,32],[39,78,33],[39,78,34],[39,78,35],[39,78,36],[39,78,37],[39,78,38],[39,79,31],[40,57,53],[40,57,54],[40,58,50],[40,58,51],[40,58,52],[40,58,53],[40,58,54],[40,58,55],[40,58,56],[40,59,50],[40,59,51],[40,59,52],[40,59,53],[40,59,54],[40,59,55],[40,59,56],[40,60,50],[40,60,51],[40,60,52],[40,60,53],[40,60,54],[40,60,55],[40,60,56],[40,61,48],[40,61,49],[40,61,50],[40,61,51],[40,61,52],[40,61,53],[40,62,48],[40,62,49],[40,62,50],[40,62,51],[40,62,52],[40,62,53],[40,63,47],[40,63,48],[40,63,49],[40,63,50],[40,63,51],[40,63,52],[40,63,53],[40,64,47],[40,64,48],[40,64,49],[40,64,50],[40,64,51],[40,64,52],[40,64,53],[40,65,45],[40,65,46],[40,65,47],[40,65,48],[40,65,49],[40,65,50],[40,65,51],[40,65,52],[40,65,53],[40,66,45],[40,66,46],[40,66,47],[40,66,48],[40,66,49],[40,66,50],[40,66,51],[40,67,27],[40,67,28],[40,67,29],[40,67,45],[40,67,46],[40,67,47],[40,67,48],[40,67,49],[40,67,50],[40,67,51],[40,68,27],[40,68,28],[40,68,29],[40,68,45],[40,68,46],[40,68,47],[40,68,48],[40,69,27],[40,69,28],[40,69,29],[40,69,30],[40,69,41],[40,69,42],[40,69,43],[40,69,45],[40,69,46],[40,69,47],[40,69,48],[40,70,27],[40,70,28],[40,70,29],[40,70,30],[40,70,41],[40,70,42],[40,70,43],[40,70,45],[40,70,46],[40,71,27],[40,71,28],[40,71,29],[40,71,30],[40,71,31],[40,71,32],[40,71,37],[40,71,38],[40,71,39],[40,71,40],[40,71,41],[40,71,42],[40,71,43],[40,71,44],[40,71,45],[40,71,46],[40,72,27],[40,72,28],[40,72,29],[40,72,30],[40,72,31],[40,72,32],[40,72,37],[40,72,38],[40,72,39],[40,72,40],[40,72,41],[40,72,42],[40,72,43],[40,72,44],[40,72,45],[40,72,46],[40,73,27],[40,73,28],[40,73,29],[40,73,30],[40,73,31],[40,73,32],[40,73,36],[40,73,37],[40,73,38],[40,73,39],[40,73,40],[40,73,41],[40,73,42],[40,74,27],[40,74,28],[40,74,29],[40,74,30],[40,74,31],[40,74,32],[40,74,33],[40,74,34],[40,74,35],[40,74,36],[40,74,37],[40,74,38],[40,74,39],[40,74,40],[40,75,29],[40,75,30],[40,75,31],[40,75,32],[40,75,33],[40,75,34],[40,75,35],[40,75,36],[40,75,37],[40,75,38],[40,75,39],[40,75,40],[40,76,29],[40,76,30],[40,76,31],[40,76,32],[40,76,33],[40,76,34],[40,76,35],[40,76,36],[40,76,37],[40,76,38],[40,77,30],[40,77,31],[40,77,32],[40,77,33],[40,77,34],[40,77,35],[40,77,36],[40,77,37],[40,77,38],[40,78,31],[40,78,32],[40,78,33],[40,78,34],[40,78,35],[40,78,36],[40,78,37],[40,78,38],[41,57,52],[41,57,53],[41,57,54],[41,57,55],[41,58,50],[41,58,51],[41,58,52],[41,58,53],[41,58,54],[41,58,55],[41,58,56],[41,59,49],[41,59,50],[41,59,51],[41,59,52],[41,59,53],[41,59,54],[41,59,55],[41,59,56],[41,60,49],[41,60,50],[41,60,51],[41,60,52],[41,60,53],[41,60,54],[41,60,55],[41,60,56],[41,61,47],[41,61,48],[41,61,49],[41,61,50],[41,61,51],[41,61,52],[41,61,53],[41,62,46],[41,62,47],[41,62,48],[41,62,49],[41,62,50],[41,62,51],[41,62,52],[41,62,53],[41,63,46],[41,63,47],[41,63,48],[41,63,49],[41,63,50],[41,63,51],[41,63,52],[41,63,53],[41,64,47],[41,64,48],[41,64,49],[41,64,50],[41,64,51],[41,64,52],[41,64,53],[41,65,28],[41,65,29],[41,65,45],[41,65,46],[41,65,47],[41,65,48],[41,65,49],[41,65,50],[41,65,51],[41,65,52],[41,65,53],[41,66,27],[41,66,28],[41,66,29],[41,66,45],[41,66,46],[41,66,47],[41,66,48],[41,66,49],[41,66,50],[41,66,51],[41,66,52],[41,66,53],[41,67,27],[41,67,28],[41,67,29],[41,67,45],[41,67,46],[41,67,47],[41,67,48],[41,67,49],[41,67,50],[41,67,51],[41,68,27],[41,68,28],[41,68,29],[41,68,45],[41,68,46],[41,68,47],[41,68,48],[41,69,27],[41,69,28],[41,69,29],[41,69,30],[41,69,41],[41,69,42],[41,69,43],[41,69,44],[41,69,45],[41,69,46],[41,69,47],[41,69,48],[41,70,27],[41,70,28],[41,70,29],[41,70,30],[41,70,31],[41,70,41],[41,70,42],[41,70,43],[41,70,44],[41,70,45],[41,70,46],[41,70,47],[41,71,27],[41,71,28],[41,71,29],[41,71,30],[41,71,31],[41,71,32],[41,71,37],[41,71,38],[41,71,39],[41,71,40],[41,71,41],[41,71,42],[41,71,43],[41,71,44],[41,71,45],[41,71,46],[41,71,47],[41,72,27],[41,72,28],[41,72,29],[41,72,30],[41,72,31],[41,72,32],[41,72,37],[41,72,38],[41,72,39],[41,72,40],[41,72,41],[41,72,42],[41,72,43],[41,72,44],[41,72,45],[41,72,46],[41,73,27],[41,73,28],[41,73,29],[41,73,30],[41,73,31],[41,73,32],[41,73,33],[41,73,34],[41,73,35],[41,73,36],[41,73,37],[41,73,38],[41,73,39],[41,73,40],[41,73,41],[41,73,42],[41,74,27],[41,74,28],[41,74,29],[41,74,30],[41,74,31],[41,74,32],[41,74,33],[41,74,34],[41,74,35],[41,74,36],[41,74,37],[41,74,38],[41,74,39],[41,74,40],[41,74,41],[41,74,42],[41,75,29],[41,75,30],[41,75,31],[41,75,32],[41,75,33],[41,75,34],[41,75,35],[41,75,36],[41,75,37],[41,75,38],[41,75,39],[41,75,40],[41,76,29],[41,76,30],[41,76,31],[41,76,32],[41,76,33],[41,76,34],[41,76,35],[41,76,36],[41,76,37],[41,76,38],[41,76,39],[41,76,40],[41,77,29],[41,77,30],[41,77,31],[41,77,32],[41,77,33],[41,77,34],[41,77,35],[41,77,36],[41,77,37],[41,77,38],[41,78,31],[41,78,32],[41,78,33],[41,78,34],[41,78,35],[41,78,36],[41,78,37],[41,78,38],[41,79,31],[42,57,50],[42,57,51],[42,57,52],[42,57,53],[42,57,54],[42,57,55],[42,58,50],[42,58,51],[42,58,52],[42,58,53],[42,58,54],[42,58,55],[42,58,56],[42,59,49],[42,59,50],[42,59,51],[42,59,52],[42,59,53],[42,59,54],[42,59,55],[42,59,56],[42,60,49],[42,60,50],[42,60,51],[42,60,52],[42,60,53],[42,60,54],[42,60,55],[42,60,56],[42,61,47],[42,61,48],[42,61,49],[42,61,50],[42,61,51],[42,61,52],[42,61,53],[42,62,46],[42,62,47],[42,62,48],[42,62,49],[42,62,50],[42,62,51],[42,62,52],[42,62,53],[42,63,46],[42,63,47],[42,63,48],[42,63,49],[42,63,50],[42,63,51],[42,63,52],[42,63,53],[42,64,27],[42,64,28],[42,64,29],[42,64,45],[42,64,46],[42,64,47],[42,64,48],[42,64,49],[42,64,50],[42,64,51],[42,64,52],[42,64,53],[42,65,27],[42,65,28],[42,65,29],[42,65,45],[42,65,46],[42,65,47],[42,65,48],[42,65,49],[42,65,50],[42,65,51],[42,65,52],[42,65,53],[42,66,27],[42,66,28],[42,66,29],[42,66,45],[42,66,46],[42,66,47],[42,66,48],[42,66,49],[42,66,50],[42,66,51],[42,66,52],[42,66,53],[42,67,27],[42,67,28],[42,67,29],[42,67,45],[42,67,46],[42,67,47],[42,67,48],[42,67,49],[42,67,50],[42,67,51],[42,68,27],[42,68,45],[42,68,46],[42,68,47],[42,68,48],[42,69,27],[42,69,28],[42,69,29],[42,69,30],[42,69,41],[42,69,42],[42,69,43],[42,69,44],[42,69,45],[42,69,46],[42,69,47],[42,69,48],[42,70,27],[42,70,28],[42,70,29],[42,70,30],[42,70,31],[42,70,41],[42,70,42],[42,70,43],[42,70,44],[42,70,45],[42,70,46],[42,70,47],[42,71,27],[42,71,28],[42,71,29],[42,71,30],[42,71,31],[42,71,32],[42,71,37],[42,71,38],[42,71,39],[42,71,40],[42,71,41],[42,71,42],[42,71,43],[42,71,44],[42,71,45],[42,71,46],[42,71,47],[42,72,27],[42,72,28],[42,72,29],[42,72,30],[42,72,31],[42,72,32],[42,72,37],[42,72,38],[42,72,39],[42,72,40],[42,72,41],[42,72,42],[42,72,43],[42,72,44],[42,72,45],[42,72,46],[42,73,27],[42,73,28],[42,73,29],[42,73,30],[42,73,31],[42,73,32],[42,73,33],[42,73,34],[42,73,35],[42,73,36],[42,73,37],[42,73,38],[42,73,39],[42,73,40],[42,73,41],[42,73,42],[42,73,43],[42,73,44],[42,73,45],[42,74,27],[42,74,28],[42,74,29],[42,74,30],[42,74,31],[42,74,32],[42,74,33],[42,74,34],[42,74,35],[42,74,36],[42,74,37],[42,74,38],[42,74,39],[42,74,40],[42,74,41],[42,74,42],[42,74,43],[42,75,29],[42,75,30],[42,75,31],[42,75,32],[42,75,33],[42,75,34],[42,75,35],[42,75,36],[42,75,37],[42,75,38],[42,75,39],[42,75,40],[42,76,29],[42,76,30],[42,76,31],[42,76,32],[42,76,33],[42,76,34],[42,76,35],[42,76,36],[42,76,37],[42,76,38],[42,76,39],[42,76,40],[42,77,29],[42,77,30],[42,77,31],[42,77,32],[42,77,33],[42,77,34],[42,77,35],[42,77,36],[42,77,37],[42,77,38],[42,78,29],[42,78,30],[42,78,31],[42,78,32],[42,78,33],[42,78,34],[42,78,35],[42,78,36],[42,78,37],[42,78,38],[43,57,50],[43,57,51],[43,57,52],[43,57,53],[43,57,54],[43,57,55],[43,58,50],[43,58,51],[43,58,52],[43,58,53],[43,58,54],[43,58,55],[43,58,56],[43,59,49],[43,59,50],[43,59,51],[43,59,52],[43,59,53],[43,59,54],[43,59,55],[43,59,56],[43,60,48],[43,60,49],[43,60,50],[43,60,51],[43,60,52],[43,60,53],[43,60,54],[43,60,55],[43,60,56],[43,61,47],[43,61,48],[43,61,49],[43,61,50],[43,62,46],[43,62,47],[43,62,48],[43,62,49],[43,62,50],[43,62,51],[43,62,52],[43,62,53],[43,63,45],[43,63,46],[43,63,47],[43,63,48],[43,63,50],[43,63,51],[43,63,52],[43,63,53],[43,64,27],[43,64,28],[43,64,29],[43,64,45],[43,64,46],[43,64,47],[43,64,48],[43,64,50],[43,64,51],[43,64,52],[43,64,53],[43,65,27],[43,65,28],[43,65,29],[43,65,45],[43,65,46],[43,65,47],[43,65,48],[43,65,49],[43,65,50],[43,65,51],[43,65,52],[43,65,53],[43,66,27],[43,66,28],[43,66,29],[43,66,43],[43,66,44],[43,66,45],[43,66,46],[43,66,47],[43,66,48],[43,66,49],[43,66,50],[43,66,51],[43,66,52],[43,66,53],[43,67,27],[43,67,28],[43,67,29],[43,67,43],[43,67,44],[43,67,45],[43,67,46],[43,67,47],[43,67,48],[43,67,49],[43,67,50],[43,67,51],[43,68,43],[43,68,44],[43,68,45],[43,68,46],[43,68,47],[43,68,48],[43,69,41],[43,69,42],[43,69,43],[43,69,44],[43,69,45],[43,69,46],[43,70,27],[43,70,28],[43,70,29],[43,70,30],[43,70,31],[43,70,39],[43,70,40],[43,70,41],[43,70,42],[43,70,43],[43,70,44],[43,70,45],[43,70,46],[43,70,47],[43,71,27],[43,71,28],[43,71,29],[43,71,30],[43,71,31],[43,71,37],[43,71,38],[43,71,39],[43,71,40],[43,71,41],[43,71,42],[43,71,43],[43,71,44],[43,71,45],[43,71,46],[43,71,47],[43,72,27],[43,72,28],[43,72,29],[43,72,30],[43,72,31],[43,72,32],[43,72,37],[43,72,38],[43,72,39],[43,72,40],[43,72,41],[43,72,42],[43,72,43],[43,72,44],[43,72,45],[43,72,46],[43,73,27],[43,73,28],[43,73,29],[43,73,30],[43,73,31],[43,73,32],[43,73,33],[43,73,34],[43,73,35],[43,73,36],[43,73,37],[43,73,38],[43,73,39],[43,73,40],[43,73,41],[43,73,42],[43,73,43],[43,73,44],[43,73,45],[43,74,27],[43,74,28],[43,74,29],[43,74,30],[43,74,31],[43,74,32],[43,74,33],[43,74,34],[43,74,35],[43,74,36],[43,74,37],[43,74,38],[43,74,39],[43,74,40],[43,74,41],[43,74,42],[43,74,43],[43,75,30],[43,75,31],[43,75,32],[43,75,33],[43,75,34],[43,75,35],[43,75,36],[43,75,37],[43,75,38],[43,75,39],[43,75,40],[43,76,29],[43,76,30],[43,76,31],[43,76,32],[43,76,33],[43,76,34],[43,76,35],[43,76,36],[43,76,37],[43,76,38],[43,76,39],[43,76,40],[43,77,29],[43,77,30],[43,77,31],[43,77,32],[43,77,33],[43,77,34],[43,77,35],[43,77,36],[43,77,37],[43,77,38],[43,78,29],[43,78,30],[43,78,31],[43,78,32],[43,78,33],[43,78,34],[43,78,35],[43,78,36],[44,57,50],[44,57,51],[44,57,52],[44,57,53],[44,58,50],[44,58,51],[44,58,52],[44,58,53],[44,58,54],[44,58,55],[44,58,56],[44,59,49],[44,59,50],[44,59,51],[44,59,52],[44,59,53],[44,59,54],[44,59,55],[44,59,56],[44,60,48],[44,60,49],[44,60,50],[44,60,52],[44,60,53],[44,60,54],[44,60,55],[44,60,56],[44,61,47],[44,61,48],[44,61,49],[44,61,50],[44,62,46],[44,62,47],[44,62,48],[44,62,49],[44,62,50],[44,63,45],[44,63,46],[44,63,47],[44,63,48],[44,63,50],[44,63,51],[44,63,52],[44,63,53],[44,64,27],[44,64,28],[44,64,29],[44,64,45],[44,64,46],[44,64,47],[44,64,48],[44,64,50],[44,64,51],[44,64,52],[44,64,53],[44,65,27],[44,65,28],[44,65,29],[44,65,43],[44,65,45],[44,65,46],[44,65,47],[44,65,48],[44,65,49],[44,65,50],[44,65,51],[44,65,52],[44,65,53],[44,66,27],[44,66,28],[44,66,29],[44,66,43],[44,66,44],[44,66,45],[44,66,46],[44,66,47],[44,66,48],[44,66,49],[44,66,50],[44,66,51],[44,66,52],[44,66,53],[44,67,43],[44,67,44],[44,67,45],[44,67,47],[44,67,48],[44,68,43],[44,68,44],[44,68,45],[44,68,48],[44,69,43],[44,69,44],[44,69,45],[44,69,46],[44,69,47],[44,70,27],[44,70,28],[44,70,29],[44,70,30],[44,70,39],[44,70,40],[44,70,41],[44,70,45],[44,70,46],[44,70,47],[44,71,27],[44,71,28],[44,71,29],[44,71,30],[44,71,37],[44,71,38],[44,71,39],[44,71,40],[44,71,41],[44,71,42],[44,71,43],[44,71,44],[44,71,45],[44,71,46],[44,72,27],[44,72,28],[44,72,29],[44,72,30],[44,72,37],[44,72,39],[44,72,40],[44,72,41],[44,72,42],[44,72,43],[44,72,44],[44,72,45],[44,72,46],[44,73,27],[44,73,28],[44,73,29],[44,73,37],[44,73,38],[44,73,39],[44,73,40],[44,73,41],[44,73,42],[44,73,43],[44,73,44],[44,73,45],[44,74,27],[44,74,28],[44,74,29],[44,74,32],[44,74,33],[44,74,34],[44,74,35],[44,74,36],[44,74,37],[44,74,38],[44,74,39],[44,74,40],[44,74,41],[44,74,42],[44,74,43],[44,74,44],[44,75,30],[44,75,31],[44,75,32],[44,75,33],[44,75,34],[44,75,35],[44,75,36],[44,75,37],[44,75,38],[44,75,39],[44,75,40],[44,76,29],[44,76,30],[44,76,31],[44,76,32],[44,76,33],[44,76,34],[44,76,35],[44,76,36],[44,76,37],[44,76,38],[44,76,40],[44,77,29],[44,77,30],[44,77,31],[44,77,32],[44,77,33],[44,77,34],[44,77,35],[44,77,36],[44,77,37],[44,77,38],[44,78,29],[44,78,30],[44,78,31],[44,78,32],[45,57,50],[45,57,51],[45,57,52],[45,57,53],[45,58,50],[45,58,51],[45,58,52],[45,58,53],[45,59,49],[45,59,50],[45,59,51],[45,59,52],[45,59,53],[45,59,54],[45,59,55],[45,59,56],[45,60,48],[45,60,49],[45,60,50],[45,60,53],[45,60,54],[45,60,55],[45,60,56],[45,61,47],[45,61,48],[45,61,49],[45,61,50],[45,61,53],[45,61,54],[45,61,55],[45,61,56],[45,62,46],[45,62,47],[45,62,48],[45,62,49],[45,62,50],[45,63,45],[45,63,46],[45,63,47],[45,63,48],[45,63,50],[45,63,51],[45,63,52],[45,63,53],[45,64,46],[45,64,47],[45,64,48],[45,64,50],[45,64,51],[45,64,52],[45,64,53],[45,65,43],[45,65,46],[45,65,47],[45,65,48],[45,65,49],[45,65,50],[45,65,51],[45,65,52],[45,65,53],[45,66,43],[45,66,44],[45,66,45],[45,66,47],[45,66,48],[45,66,49],[45,66,50],[45,66,51],[45,66,52],[45,66,53],[45,67,43],[45,67,44],[45,67,45],[45,67,47],[45,67,48],[45,68,43],[45,68,44],[45,68,45],[45,68,46],[45,69,45],[45,69,46],[45,69,47],[45,70,27],[45,70,28],[45,70,29],[45,70,30],[45,70,39],[45,70,40],[45,70,41],[45,70,42],[45,70,43],[45,70,45],[45,70,46],[45,70,47],[45,71,27],[45,71,28],[45,71,29],[45,71,30],[45,71,39],[45,71,40],[45,71,41],[45,71,42],[45,71,43],[45,71,44],[45,71,45],[45,71,46],[45,72,27],[45,72,28],[45,72,29],[45,72,30],[45,72,37],[45,72,38],[45,72,39],[45,72,40],[45,72,41],[45,72,42],[45,72,43],[45,72,45],[45,72,46],[45,73,37],[45,73,38],[45,73,39],[45,73,40],[45,73,41],[45,73,42],[45,74,34],[45,74,35],[45,74,36],[45,74,37],[45,74,38],[45,74,39],[45,74,40],[45,74,41],[45,75,30],[45,75,31],[45,75,32],[45,75,34],[45,75,35],[45,75,36],[45,75,37],[45,75,38],[45,75,39],[45,75,40],[45,76,29],[45,76,30],[45,76,31],[45,76,32],[45,76,34],[45,76,35],[45,76,36],[45,76,37],[45,76,38],[45,77,29],[45,77,30],[45,77,31],[45,77,32],[45,78,29],[45,78,30],[45,78,31],[45,78,32],[46,57,50],[46,57,51],[46,57,52],[46,57,53],[46,58,50],[46,58,51],[46,58,52],[46,58,53],[46,59,48],[46,59,49],[46,59,50],[46,59,51],[46,59,52],[46,59,53],[46,59,54],[46,59,55],[46,59,56],[46,60,48],[46,60,49],[46,60,50],[46,60,53],[46,60,54],[46,60,55],[46,60,56],[46,61,48],[46,61,49],[46,61,50],[46,61,53],[46,61,54],[46,61,55],[46,61,56],[46,62,45],[46,62,46],[46,62,47],[46,62,48],[46,62,49],[46,62,50],[46,63,45],[46,63,46],[46,63,47],[46,63,48],[46,63,51],[46,63,52],[46,63,53],[46,64,45],[46,64,46],[46,64,47],[46,64,48],[46,64,50],[46,64,51],[46,64,52],[46,64,53],[46,65,43],[46,65,44],[46,65,45],[46,65,46],[46,65,47],[46,65,48],[46,65,49],[46,65,50],[46,65,51],[46,65,52],[46,65,53],[46,66,43],[46,66,44],[46,66,45],[46,66,46],[46,66,47],[46,66,48],[46,66,49],[46,66,50],[46,66,51],[46,66,52],[46,66,53],[46,67,43],[46,67,44],[46,67,45],[46,67,46],[46,67,47],[46,67,48],[46,67,49],[46,68,45],[46,68,46],[46,68,47],[46,69,45],[46,69,46],[46,69,47],[46,70,27],[46,70,28],[46,70,29],[46,70,30],[46,70,41],[46,70,45],[46,70,46],[46,71,27],[46,71,28],[46,71,29],[46,71,30],[46,71,39],[46,71,40],[46,71,41],[46,71,42],[46,71,43],[46,71,44],[46,71,45],[46,72,27],[46,72,28],[46,72,29],[46,72,30],[46,72,37],[46,72,38],[46,72,39],[46,72,40],[46,72,41],[46,72,42],[46,72,43],[46,72,44],[46,73,37],[46,73,38],[46,73,39],[46,73,40],[46,73,41],[46,73,42],[46,74,34],[46,74,35],[46,74,36],[46,74,37],[46,74,38],[46,74,39],[46,74,40],[46,75,29],[46,75,32],[46,75,34],[46,75,35],[46,75,36],[46,75,37],[46,75,38],[46,76,29],[46,76,30],[46,76,31],[46,76,32],[46,76,34],[46,76,35],[46,76,36],[46,76,37],[46,76,38],[46,77,29],[46,77,30],[46,77,31],[46,77,32],[46,78,29],[46,78,30],[46,78,31],[46,78,32],[46,79,30],[46,79,31],[47,57,50],[47,57,51],[47,57,52],[47,57,53],[47,58,50],[47,58,51],[47,58,52],[47,58,53],[47,59,48],[47,59,49],[47,59,50],[47,59,51],[47,59,52],[47,59,53],[47,59,54],[47,59,55],[47,59,56],[47,60,48],[47,60,49],[47,60,50],[47,60,53],[47,60,54],[47,60,55],[47,60,56],[47,61,49],[47,61,50],[47,61,53],[47,61,54],[47,61,55],[47,61,56],[47,62,47],[47,62,48],[47,62,49],[47,62,50],[47,63,47],[47,63,48],[47,64,47],[47,64,48],[47,65,47],[47,65,48],[47,65,49],[47,65,50],[47,66,46],[47,66,47],[47,66,48],[47,66,49],[47,66,50],[47,66,51],[47,67,45],[47,67,46],[47,67,47],[47,67,48],[47,67,49],[47,67,50],[47,67,51],[47,68,45],[47,68,46],[47,68,47],[47,69,41],[47,69,42],[47,69,43],[47,69,45],[47,69,46],[47,69,47],[47,70,41],[47,70,42],[47,70,43],[47,70,44],[47,70,45],[47,71,39],[47,71,40],[47,71,41],[47,71,42],[47,71,43],[47,71,44],[47,72,37],[47,72,38],[47,72,39],[47,72,40],[47,72,41],[47,72,42],[47,72,43],[47,72,44],[47,72,45],[47,73,37],[47,73,38],[47,73,39],[47,73,40],[47,74,34],[47,74,35],[47,74,36],[47,74,37],[47,74,38],[47,74,39],[47,74,40],[47,75,29],[47,75,32],[47,75,34],[47,75,35],[47,75,36],[47,75,37],[47,75,38],[47,76,29],[47,76,30],[47,76,31],[47,76,32],[47,76,34],[47,76,35],[47,76,36],[47,76,37],[47,76,38],[47,77,29],[47,77,30],[47,77,31],[47,77,32],[47,77,36],[47,77,37],[47,77,38],[47,78,30],[47,78,31],[48,57,50],[48,57,51],[48,57,52],[48,58,53],[48,58,54],[48,58,55],[48,58,56],[48,59,48],[48,59,49],[48,59,50],[48,59,53],[48,59,54],[48,59,55],[48,59,56],[48,60,48],[48,60,49],[48,60,50],[48,60,53],[48,60,54],[48,60,55],[48,60,56],[48,61,48],[48,61,49],[48,61,50],[48,61,53],[48,61,54],[48,61,55],[48,62,48],[48,62,49],[48,62,50],[48,63,48],[48,64,47],[48,64,48],[48,65,48],[48,65,49],[48,65,50],[48,66,47],[48,66,48],[48,66,49],[48,66,50],[48,66,51],[48,67,47],[48,67,48],[48,67,49],[48,67,50],[48,67,51],[48,68,45],[48,68,46],[48,69,45],[48,69,46],[48,70,41],[48,70,42],[48,70,43],[48,70,44],[48,70,45],[48,70,46],[48,71,39],[48,71,40],[48,71,41],[48,71,42],[48,71,43],[48,71,44],[48,71,45],[48,72,38],[48,72,39],[48,72,40],[48,72,41],[48,72,42],[48,72,43],[48,72,44],[48,72,45],[48,73,37],[48,73,38],[48,73,39],[48,73,40],[48,73,41],[48,74,34],[48,74,35],[48,74,36],[48,74,37],[48,74,38],[48,74,39],[48,74,40],[48,75,29],[48,75,30],[48,75,31],[48,75,32],[48,75,34],[48,75,35],[48,75,36],[48,75,37],[48,75,38],[48,76,29],[48,76,30],[48,76,31],[48,76,34],[48,76,35],[48,76,36],[48,76,37],[48,76,38],[48,77,30],[48,77,31],[48,77,36],[48,77,37],[48,77,38],[49,58,53],[49,58,54],[49,58,55],[49,58,56],[49,59,53],[49,59,54],[49,59,55],[49,59,56],[49,60,53],[49,60,54],[49,60,55],[49,60,56],[49,70,41],[49,70,42],[49,70,43],[49,70,44],[49,70,45],[49,71,41],[49,71,42],[49,71,43],[49,71,44],[49,71,45],[49,72,39],[49,72,40],[49,72,41],[49,72,42],[49,72,43],[49,72,44],[49,72,45],[49,73,37],[49,73,38],[49,73,39],[49,73,40],[49,73,41],[49,74,38],[49,74,39],[49,74,40],[49,75,36],[49,75,37],[49,75,38],[49,76,36],[49,76,37],[49,76,38],[49,77,36],[49,77,37],[49,77,38],[50,58,53],[50,58,54],[50,58,55],[50,59,53],[50,59,54],[50,59,55],[50,60,53],[50,60,54],[50,60,55],[50,72,39],[50,72,40],[50,72,41],[50,72,43],[50,73,38],[50,73,39],[50,73,40],[50,73,41],[50,74,38],[50,74,39],[50,74,40]],"colors":[[0,0,0,1]],"centers":[[30,72,41],[30,73,38],[30,73,39],[31,58,53],[31,58,54],[31,58,55],[31,58,56],[31,59,53],[31,59,54],[31,59,55],[31,59,56],[31,60,53],[31,60,54],[31,60,55],[31,60,56],[31,70,41],[31,70,42],[31,70,43],[31,70,44],[31,71,39],[31,71,40],[31,71,41],[31,71,42],[31,71,43],[31,71,44],[31,72,39],[31,72,40],[31,72,41],[31,72,42],[31,72,43],[31,72,44],[31,72,45],[31,73,37],[31,73,38],[31,73,39],[31,73,40],[31,73,41],[31,73,42],[31,74,38],[31,74,39],[31,74,40],[31,75,36],[31,75,37],[31,75,38],[31,76,36],[31,76,37],[31,76,38],[31,77,36],[31,77,37],[31,77,38],[32,57,50],[32,57,51],[32,57,52],[32,57,53],[32,58,51],[32,58,53],[32,58,54],[32,58,55],[32,58,56],[32,59,53],[32,59,54],[32,59,55],[32,59,56],[32,60,48],[32,60,49],[32,60,50],[32,60,53],[32,60,54],[32,60,55],[32,60,56],[32,61,48],[32,61,49],[32,61,50],[32,62,48],[32,62,49],[32,62,50],[32,65,48],[32,65,49],[32,65,50],[32,66,47],[32,66,48],[32,66,49],[32,66,50],[32,66,51],[32,67,47],[32,67,48],[32,67,49],[32,67,50],[32,67,51],[32,70,41],[32,70,42],[32,70,43],[32,70,44],[32,71,39],[32,71,40],[32,71,41],[32,71,42],[32,71,43],[32,71,44],[32,72,38],[32,72,39],[32,72,40],[32,72,41],[32,72,42],[32,72,43],[32,72,44],[32,72,45],[32,73,37],[32,73,38],[32,73,39],[32,73,40],[32,73,41],[32,74,37],[32,74,38],[32,74,39],[32,74,40],[32,74,41],[32,75,29],[32,75,30],[32,75,31],[32,75,32],[32,75,36],[32,76,29],[32,76,30],[32,76,31],[32,76,34],[32,76,35],[32,76,36],[32,77,30],[32,77,31],[33,57,50],[33,57,51],[33,57,52],[33,57,53],[33,58,50],[33,58,51],[33,58,52],[33,58,53],[33,59,48],[33,59,49],[33,59,50],[33,59,51],[33,59,52],[33,59,53],[33,59,54],[33,59,55],[33,59,56],[33,60,48],[33,60,49],[33,60,50],[33,60,53],[33,60,54],[33,60,55],[33,60,56],[33,61,48],[33,61,49],[33,61,50],[33,61,53],[33,61,54],[33,61,55],[33,61,56],[33,62,47],[33,62,48],[33,62,49],[33,62,50],[33,63,47],[33,63,48],[33,64,47],[33,64,48],[33,65,48],[33,65,49],[33,65,50],[33,66,47],[33,66,48],[33,66,49],[33,66,50],[33,66,51],[33,67,45],[33,67,46],[33,67,47],[33,67,48],[33,67,49],[33,67,50],[33,67,51],[33,68,45],[33,68,46],[33,68,47],[33,69,45],[33,69,46],[33,69,47],[33,70,41],[33,70,42],[33,70,43],[33,70,44],[33,70,45],[33,70,46],[33,71,39],[33,71,40],[33,71,41],[33,71,42],[33,71,43],[33,71,44],[33,72,37],[33,72,38],[33,72,39],[33,72,40],[33,72,41],[33,72,42],[33,72,43],[33,72,44],[33,72,45],[33,73,37],[33,73,38],[33,73,39],[33,73,40],[33,74,34],[33,74,35],[33,74,36],[33,74,37],[33,74,38],[33,74,39],[33,74,40],[33,75,29],[33,75,30],[33,75,31],[33,75,32],[33,75,34],[33,75,35],[33,75,36],[33,75,37],[33,75,38],[33,76,29],[33,76,30],[33,76,31],[33,76,32],[33,76,34],[33,76,35],[33,76,36],[33,76,37],[33,76,38],[33,77,29],[33,77,30],[33,77,31],[33,77,32],[33,78,29],[33,78,30],[33,78,31],[33,78,32],[34,57,50],[34,57,51],[34,57,52],[34,57,53],[34,58,50],[34,58,51],[34,58,52],[34,58,53],[34,59,48],[34,59,49],[34,59,50],[34,59,51],[34,59,52],[34,59,53],[34,59,54],[34,59,55],[34,59,56],[34,60,48],[34,60,49],[34,60,50],[34,60,53],[34,60,54],[34,60,55],[34,60,56],[34,61,48],[34,61,49],[34,61,50],[34,61,53],[34,61,54],[34,61,55],[34,61,56],[34,62,47],[34,62,48],[34,62,49],[34,62,50],[34,63,47],[34,63,48],[34,64,47],[34,64,48],[34,65,46],[34,65,47],[34,65,48],[34,65,49],[34,65,50],[34,65,51],[34,65,52],[34,65,53],[34,66,46],[34,66,47],[34,66,48],[34,66,49],[34,66,50],[34,66,51],[34,66,52],[34,67,45],[34,67,46],[34,67,47],[34,67,48],[34,67,49],[34,67,50],[34,67,51],[34,68,45],[34,68,46],[34,69,45],[34,69,46],[34,70,41],[34,70,42],[34,70,43],[34,70,44],[34,70,45],[34,70,46],[34,71,39],[34,71,40],[34,71,41],[34,71,42],[34,71,43],[34,71,44],[34,71,45],[34,72,37],[34,72,38],[34,72,39],[34,72,40],[34,72,41],[34,72,42],[34,72,43],[34,72,44],[34,72,45],[34,73,37],[34,73,38],[34,73,39],[34,73,40],[34,74,34],[34,74,35],[34,74,36],[34,74,37],[34,74,40],[34,75,29],[34,75,32],[34,75,34],[34,75,35],[34,75,36],[34,75,37],[34,75,38],[34,76,29],[34,76,30],[34,76,31],[34,76,32],[34,76,34],[34,76,35],[34,76,36],[34,76,37],[34,76,38],[34,77,29],[34,77,30],[34,77,31],[34,77,32],[34,78,29],[34,78,30],[34,78,31],[34,78,32],[34,79,30],[34,79,31],[35,57,50],[35,57,51],[35,57,52],[35,57,53],[35,58,50],[35,58,51],[35,58,52],[35,58,53],[35,59,48],[35,59,49],[35,59,50],[35,59,51],[35,59,52],[35,59,53],[35,59,54],[35,59,55],[35,59,56],[35,60,48],[35,60,49],[35,60,50],[35,60,53],[35,60,54],[35,60,55],[35,60,56],[35,61,48],[35,61,49],[35,61,50],[35,61,53],[35,61,54],[35,61,55],[35,61,56],[35,62,46],[35,62,47],[35,62,48],[35,62,49],[35,62,50],[35,63,45],[35,63,46],[35,63,47],[35,63,48],[35,63,50],[35,63,51],[35,63,52],[35,63,53],[35,64,45],[35,64,46],[35,64,47],[35,64,48],[35,64,50],[35,64,51],[35,64,52],[35,64,53],[35,65,43],[35,65,44],[35,65,45],[35,65,46],[35,65,47],[35,65,48],[35,65,49],[35,65,50],[35,65,51],[35,65,52],[35,65,53],[35,66,43],[35,66,44],[35,66,47],[35,66,48],[35,66,49],[35,66,50],[35,66,51],[35,66,52],[35,67,43],[35,67,44],[35,67,45],[35,67,46],[35,67,47],[35,67,48],[35,68,43],[35,68,44],[35,68,45],[35,68,46],[35,69,45],[35,69,46],[35,69,47],[35,70,27],[35,70,28],[35,70,29],[35,70,30],[35,70,41],[35,70,45],[35,70,46],[35,70,47],[35,71,27],[35,71,28],[35,71,29],[35,71,30],[35,71,38],[35,71,39],[35,71,40],[35,71,41],[35,71,42],[35,71,43],[35,71,44],[35,71,45],[35,71,46],[35,72,27],[35,72,28],[35,72,29],[35,72,30],[35,72,37],[35,72,38],[35,72,39],[35,72,40],[35,72,41],[35,72,42],[35,72,43],[35,73,37],[35,73,38],[35,73,39],[35,73,40],[35,73,41],[35,73,42],[35,74,32],[35,74,33],[35,74,34],[35,74,35],[35,74,36],[35,74,37],[35,74,38],[35,74,39],[35,74,40],[35,74,41],[35,75,30],[35,75,31],[35,75,32],[35,75,33],[35,75,34],[35,75,35],[35,75,36],[35,75,37],[35,75,38],[35,75,39],[35,76,29],[35,76,30],[35,76,31],[35,76,32],[35,76,33],[35,76,34],[35,76,35],[35,76,36],[35,76,37],[35,76,38],[35,77,29],[35,77,30],[35,77,31],[35,77,32],[35,77,34],[35,78,29],[35,78,30],[35,78,31],[35,78,32],[35,79,30],[35,79,31],[36,57,50],[36,57,51],[36,57,52],[36,57,53],[36,58,50],[36,58,51],[36,58,52],[36,58,53],[36,59,49],[36,59,50],[36,59,51],[36,59,52],[36,59,53],[36,59,54],[36,59,55],[36,59,56],[36,60,48],[36,60,49],[36,60,50],[36,60,53],[36,60,54],[36,60,55],[36,60,56],[36,61,47],[36,61,48],[36,61,49],[36,61,50],[36,61,53],[36,61,54],[36,61,55],[36,61,56],[36,62,47],[36,62,48],[36,62,49],[36,62,50],[36,63,45],[36,63,46],[36,63,47],[36,63,48],[36,63,50],[36,63,51],[36,63,52],[36,63,53],[36,64,45],[36,64,46],[36,64,47],[36,64,48],[36,64,50],[36,64,51],[36,64,52],[36,64,53],[36,65,43],[36,65,44],[36,65,45],[36,65,46],[36,65,47],[36,65,48],[36,65,49],[36,65,50],[36,65,51],[36,65,52],[36,65,53],[36,66,43],[36,66,44],[36,66,45],[36,66,46],[36,66,47],[36,66,48],[36,66,49],[36,66,50],[36,66,51],[36,66,52],[36,66,53],[36,67,43],[36,67,44],[36,67,45],[36,67,46],[36,67,47],[36,67,48],[36,68,43],[36,68,44],[36,68,45],[36,69,45],[36,69,46],[36,70,39],[36,70,40],[36,70,41],[36,70,42],[36,70,43],[36,70,45],[36,70,46],[36,71,27],[36,71,28],[36,71,37],[36,71,38],[36,71,39],[36,71,40],[36,71,41],[36,71,42],[36,71,43],[36,71,44],[36,71,45],[36,71,46],[36,72,27],[36,72,28],[36,72,29],[36,72,30],[36,72,37],[36,72,39],[36,72,40],[36,72,41],[36,72,42],[36,72,43],[36,72,45],[36,72,46],[36,73,37],[36,73,38],[36,73,39],[36,73,40],[36,73,41],[36,73,42],[36,73,43],[36,74,32],[36,74,33],[36,74,34],[36,74,35],[36,74,36],[36,74,37],[36,74,38],[36,74,39],[36,74,40],[36,74,41],[36,75,30],[36,75,31],[36,75,32],[36,75,33],[36,75,34],[36,75,35],[36,75,36],[36,75,37],[36,75,38],[36,75,39],[36,75,40],[36,76,29],[36,76,30],[36,76,31],[36,76,32],[36,76,33],[36,76,34],[36,76,35],[36,76,36],[36,76,37],[36,76,38],[36,76,39],[36,77,29],[36,77,30],[36,77,31],[36,77,32],[36,77,33],[36,77,34],[36,78,29],[36,78,30],[36,78,31],[36,78,32],[37,57,50],[37,57,51],[37,57,52],[37,57,53],[37,57,54],[37,57,55],[37,58,50],[37,58,51],[37,58,52],[37,58,53],[37,58,54],[37,58,55],[37,58,56],[37,59,49],[37,59,50],[37,59,51],[37,59,52],[37,59,53],[37,59,54],[37,59,55],[37,59,56],[37,60,48],[37,60,49],[37,60,50],[37,60,51],[37,60,52],[37,60,53],[37,60,54],[37,60,55],[37,60,56],[37,61,47],[37,61,48],[37,61,49],[37,61,50],[37,61,51],[37,61,52],[37,61,53],[37,62,46],[37,62,47],[37,62,48],[37,62,49],[37,62,50],[37,62,51],[37,62,52],[37,62,53],[37,63,45],[37,63,46],[37,63,47],[37,63,48],[37,63,50],[37,63,51],[37,63,52],[37,63,53],[37,64,27],[37,64,28],[37,64,29],[37,64,45],[37,64,46],[37,64,47],[37,64,48],[37,64,49],[37,64,50],[37,64,51],[37,64,52],[37,64,53],[37,65,27],[37,65,28],[37,65,29],[37,65,45],[37,65,46],[37,65,47],[37,65,48],[37,65,49],[37,65,50],[37,65,51],[37,65,52],[37,65,53],[37,66,27],[37,66,28],[37,66,29],[37,66,43],[37,66,44],[37,66,45],[37,66,46],[37,66,47],[37,66,48],[37,66,49],[37,66,50],[37,66,51],[37,66,52],[37,66,53],[37,67,27],[37,67,28],[37,67,29],[37,67,43],[37,67,44],[37,67,45],[37,67,46],[37,67,47],[37,67,48],[37,67,49],[37,67,50],[37,67,51],[37,68,27],[37,68,28],[37,68,29],[37,68,43],[37,68,44],[37,68,45],[37,68,47],[37,68,48],[37,69,27],[37,69,28],[37,69,29],[37,69,30],[37,69,41],[37,69,42],[37,69,43],[37,69,44],[37,69,45],[37,70,27],[37,70,28],[37,70,29],[37,70,30],[37,70,31],[37,70,39],[37,70,40],[37,70,41],[37,70,42],[37,70,43],[37,70,44],[37,70,45],[37,70,46],[37,71,27],[37,71,28],[37,71,29],[37,71,30],[37,71,31],[37,71,32],[37,71,37],[37,71,38],[37,71,39],[37,71,40],[37,71,41],[37,71,42],[37,71,43],[37,71,44],[37,71,45],[37,71,46],[37,72,27],[37,72,28],[37,72,29],[37,72,30],[37,72,31],[37,72,32],[37,72,37],[37,72,38],[37,72,39],[37,72,40],[37,72,41],[37,72,42],[37,72,43],[37,72,44],[37,72,45],[37,72,46],[37,73,27],[37,73,28],[37,73,29],[37,73,30],[37,73,31],[37,73,32],[37,73,33],[37,73,34],[37,73,35],[37,73,36],[37,73,37],[37,73,38],[37,73,39],[37,73,40],[37,73,41],[37,73,42],[37,73,43],[37,73,44],[37,73,45],[37,74,27],[37,74,28],[37,74,29],[37,74,30],[37,74,31],[37,74,32],[37,74,33],[37,74,34],[37,74,35],[37,74,36],[37,74,37],[37,74,38],[37,74,39],[37,74,40],[37,74,41],[37,74,42],[37,74,43],[37,74,44],[37,75,29],[37,75,30],[37,75,31],[37,75,32],[37,75,33],[37,75,34],[37,75,35],[37,75,36],[37,75,37],[37,75,38],[37,75,39],[37,75,40],[37,76,29],[37,76,30],[37,76,31],[37,76,32],[37,76,33],[37,76,34],[37,76,35],[37,76,36],[37,76,37],[37,76,38],[37,76,39],[37,76,40],[37,77,29],[37,77,30],[37,77,31],[37,77,32],[37,77,33],[37,77,34],[37,77,35],[37,77,36],[37,77,37],[37,77,38],[37,78,29],[37,78,30],[37,78,31],[37,78,32],[37,78,33],[37,78,34],[37,78,35],[37,78,36],[37,78,37],[37,78,38],[37,79,31],[38,57,50],[38,57,51],[38,57,52],[38,57,53],[38,57,54],[38,57,55],[38,58,50],[38,58,51],[38,58,52],[38,58,53],[38,58,54],[38,58,55],[38,58,56],[38,59,50],[38,59,51],[38,59,52],[38,59,53],[38,59,54],[38,59,55],[38,59,56],[38,60,48],[38,60,49],[38,60,50],[38,60,51],[38,60,52],[38,60,53],[38,60,54],[38,60,55],[38,60,56],[38,61,47],[38,61,48],[38,61,49],[38,61,50],[38,61,51],[38,61,52],[38,61,53],[38,62,46],[38,62,47],[38,62,48],[38,62,49],[38,62,50],[38,62,51],[38,62,52],[38,62,53],[38,63,45],[38,63,46],[38,63,47],[38,63,48],[38,63,49],[38,63,50],[38,63,51],[38,63,52],[38,63,53],[38,64,27],[38,64,28],[38,64,29],[38,64,45],[38,64,46],[38,64,47],[38,64,48],[38,64,49],[38,64,50],[38,64,51],[38,64,52],[38,64,53],[38,65,27],[38,65,28],[38,65,29],[38,65,45],[38,65,46],[38,65,47],[38,65,48],[38,65,49],[38,65,50],[38,65,51],[38,65,52],[38,65,53],[38,66,27],[38,66,45],[38,66,46],[38,66,47],[38,66,48],[38,66,49],[38,66,50],[38,66,51],[38,66,52],[38,66,53],[38,67,27],[38,67,28],[38,67,29],[38,67,43],[38,67,45],[38,67,46],[38,67,47],[38,67,48],[38,67,49],[38,67,50],[38,67,51],[38,68,27],[38,68,28],[38,68,29],[38,68,43],[38,68,44],[38,68,45],[38,68,46],[38,68,47],[38,68,48],[38,69,27],[38,69,28],[38,69,29],[38,69,30],[38,69,41],[38,69,42],[38,69,43],[38,69,44],[38,69,45],[38,69,46],[38,69,47],[38,70,27],[38,70,28],[38,70,29],[38,70,30],[38,70,31],[38,70,40],[38,70,41],[38,70,42],[38,70,43],[38,70,44],[38,70,45],[38,70,46],[38,70,47],[38,71,27],[38,71,28],[38,71,29],[38,71,30],[38,71,31],[38,71,32],[38,71,37],[38,71,38],[38,71,39],[38,71,40],[38,71,41],[38,71,42],[38,71,43],[38,71,44],[38,71,45],[38,71,46],[38,71,47],[38,72,27],[38,72,28],[38,72,29],[38,72,30],[38,72,31],[38,72,32],[38,72,37],[38,72,38],[38,72,39],[38,72,40],[38,72,41],[38,72,42],[38,72,43],[38,72,44],[38,72,45],[38,72,46],[38,73,27],[38,73,28],[38,73,29],[38,73,30],[38,73,31],[38,73,32],[38,73,33],[38,73,34],[38,73,35],[38,73,36],[38,73,37],[38,73,38],[38,73,39],[38,73,40],[38,73,41],[38,73,42],[38,73,43],[38,73,44],[38,73,45],[38,74,27],[38,74,28],[38,74,29],[38,74,30],[38,74,31],[38,74,32],[38,74,33],[38,74,34],[38,74,35],[38,74,36],[38,74,37],[38,74,38],[38,74,39],[38,74,40],[38,74,41],[38,74,42],[38,74,43],[38,75,29],[38,75,30],[38,75,31],[38,75,32],[38,75,33],[38,75,34],[38,75,35],[38,75,36],[38,75,37],[38,75,38],[38,75,39],[38,75,40],[38,76,29],[38,76,30],[38,76,31],[38,76,32],[38,76,33],[38,76,34],[38,76,35],[38,76,36],[38,76,37],[38,76,38],[38,76,39],[38,76,40],[38,77,29],[38,77,30],[38,77,31],[38,77,32],[38,77,33],[38,77,34],[38,77,35],[38,77,36],[38,77,37],[38,77,38],[38,78,29],[38,78,30],[38,78,31],[38,78,32],[38,78,33],[38,78,34],[38,78,35],[38,78,36],[38,78,37],[38,78,38],[38,79,31],[39,57,54],[39,57,55],[39,58,50],[39,58,51],[39,58,52],[39,58,53],[39,58,54],[39,58,55],[39,58,56],[39,59,50],[39,59,51],[39,59,52],[39,59,53],[39,59,54],[39,59,55],[39,59,56],[39,60,49],[39,60,50],[39,60,51],[39,60,52],[39,60,53],[39,60,54],[39,60,55],[39,60,56],[39,61,48],[39,61,49],[39,61,50],[39,61,51],[39,61,52],[39,61,53],[39,62,47],[39,62,48],[39,62,49],[39,62,50],[39,62,51],[39,62,52],[39,62,53],[39,63,47],[39,63,48],[39,63,49],[39,63,50],[39,63,51],[39,63,52],[39,63,53],[39,64,47],[39,64,48],[39,64,49],[39,64,50],[39,64,51],[39,64,52],[39,64,53],[39,65,27],[39,65,45],[39,65,46],[39,65,47],[39,65,48],[39,65,49],[39,65,50],[39,65,51],[39,65,52],[39,65,53],[39,66,27],[39,66,45],[39,66,46],[39,66,47],[39,66,48],[39,66,49],[39,66,50],[39,66,51],[39,66,52],[39,66,53],[39,67,27],[39,67,28],[39,67,29],[39,67,45],[39,67,46],[39,67,47],[39,67,48],[39,67,49],[39,67,50],[39,67,51],[39,68,27],[39,68,28],[39,68,29],[39,68,45],[39,68,46],[39,68,47],[39,68,48],[39,69,27],[39,69,28],[39,69,29],[39,69,30],[39,69,41],[39,69,42],[39,69,43],[39,69,44],[39,69,45],[39,69,46],[39,69,47],[39,70,27],[39,70,28],[39,70,29],[39,70,30],[39,70,41],[39,70,42],[39,70,43],[39,70,44],[39,70,45],[39,70,46],[39,70,47],[39,71,27],[39,71,28],[39,71,29],[39,71,30],[39,71,31],[39,71,32],[39,71,37],[39,71,38],[39,71,39],[39,71,40],[39,71,41],[39,71,42],[39,71,43],[39,71,44],[39,71,45],[39,71,46],[39,71,47],[39,72,27],[39,72,28],[39,72,29],[39,72,30],[39,72,31],[39,72,32],[39,72,37],[39,72,38],[39,72,39],[39,72,40],[39,72,41],[39,72,42],[39,72,43],[39,72,44],[39,72,45],[39,72,46],[39,73,27],[39,73,28],[39,73,29],[39,73,30],[39,73,31],[39,73,32],[39,73,33],[39,73,34],[39,73,35],[39,73,36],[39,73,37],[39,73,38],[39,73,39],[39,73,40],[39,73,41],[39,73,42],[39,73,43],[39,73,44],[39,74,27],[39,74,28],[39,74,29],[39,74,30],[39,74,31],[39,74,32],[39,74,33],[39,74,34],[39,74,35],[39,74,36],[39,74,37],[39,74,38],[39,74,39],[39,74,40],[39,74,41],[39,75,29],[39,75,30],[39,75,31],[39,75,32],[39,75,33],[39,75,34],[39,75,35],[39,75,36],[39,75,37],[39,75,38],[39,75,39],[39,75,40],[39,76,29],[39,76,30],[39,76,31],[39,76,32],[39,76,33],[39,76,34],[39,76,35],[39,76,36],[39,76,37],[39,76,38],[39,76,39],[39,77,29],[39,77,30],[39,77,31],[39,77,32],[39,77,33],[39,77,34],[39,77,35],[39,77,36],[39,77,37],[39,77,38],[39,78,31],[39,78,32],[39,78,33],[39,78,34],[39,78,35],[39,78,36],[39,78,37],[39,78,38],[39,79,31],[40,57,53],[40,57,54],[40,58,50],[40,58,51],[40,58,52],[40,58,53],[40,58,54],[40,58,55],[40,58,56],[40,59,50],[40,59,51],[40,59,52],[40,59,53],[40,59,54],[40,59,55],[40,59,56],[40,60,50],[40,60,51],[40,60,52],[40,60,53],[40,60,54],[40,60,55],[40,60,56],[40,61,48],[40,61,49],[40,61,50],[40,61,51],[40,61,52],[40,61,53],[40,62,48],[40,62,49],[40,62,50],[40,62,51],[40,62,52],[40,62,53],[40,63,47],[40,63,48],[40,63,49],[40,63,50],[40,63,51],[40,63,52],[40,63,53],[40,64,47],[40,64,48],[40,64,49],[40,64,50],[40,64,51],[40,64,52],[40,64,53],[40,65,45],[40,65,46],[40,65,47],[40,65,48],[40,65,49],[40,65,50],[40,65,51],[40,65,52],[40,65,53],[40,66,45],[40,66,46],[40,66,47],[40,66,48],[40,66,49],[40,66,50],[40,66,51],[40,67,27],[40,67,28],[40,67,29],[40,67,45],[40,67,46],[40,67,47],[40,67,48],[40,67,49],[40,67,50],[40,67,51],[40,68,27],[40,68,28],[40,68,29],[40,68,45],[40,68,46],[40,68,47],[40,68,48],[40,69,27],[40,69,28],[40,69,29],[40,69,30],[40,69,41],[40,69,42],[40,69,43],[40,69,45],[40,69,46],[40,69,47],[40,69,48],[40,70,27],[40,70,28],[40,70,29],[40,70,30],[40,70,41],[40,70,42],[40,70,43],[40,70,45],[40,70,46],[40,71,27],[40,71,28],[40,71,29],[40,71,30],[40,71,31],[40,71,32],[40,71,37],[40,71,38],[40,71,39],[40,71,40],[40,71,41],[40,71,42],[40,71,43],[40,71,44],[40,71,45],[40,71,46],[40,72,27],[40,72,28],[40,72,29],[40,72,30],[40,72,31],[40,72,32],[40,72,37],[40,72,38],[40,72,39],[40,72,40],[40,72,41],[40,72,42],[40,72,43],[40,72,44],[40,72,45],[40,72,46],[40,73,27],[40,73,28],[40,73,29],[40,73,30],[40,73,31],[40,73,32],[40,73,36],[40,73,37],[40,73,38],[40,73,39],[40,73,40],[40,73,41],[40,73,42],[40,74,27],[40,74,28],[40,74,29],[40,74,30],[40,74,31],[40,74,32],[40,74,33],[40,74,34],[40,74,35],[40,74,36],[40,74,37],[40,74,38],[40,74,39],[40,74,40],[40,75,29],[40,75,30],[40,75,31],[40,75,32],[40,75,33],[40,75,34],[40,75,35],[40,75,36],[40,75,37],[40,75,38],[40,75,39],[40,75,40],[40,76,29],[40,76,30],[40,76,31],[40,76,32],[40,76,33],[40,76,34],[40,76,35],[40,76,36],[40,76,37],[40,76,38],[40,77,30],[40,77,31],[40,77,32],[40,77,33],[40,77,34],[40,77,35],[40,77,36],[40,77,37],[40,77,38],[40,78,31],[40,78,32],[40,78,33],[40,78,34],[40,78,35],[40,78,36],[40,78,37],[40,78,38],[41,57,52],[41,57,53],[41,57,54],[41,57,55],[41,58,50],[41,58,51],[41,58,52],[41,58,53],[41,58,54],[41,58,55],[41,58,56],[41,59,49],[41,59,50],[41,59,51],[41,59,52],[41,59,53],[41,59,54],[41,59,55],[41,59,56],[41,60,49],[41,60,50],[41,60,51],[41,60,52],[41,60,53],[41,60,54],[41,60,55],[41,60,56],[41,61,47],[41,61,48],[41,61,49],[41,61,50],[41,61,51],[41,61,52],[41,61,53],[41,62,46],[41,62,47],[41,62,48],[41,62,49],[41,62,50],[41,62,51],[41,62,52],[41,62,53],[41,63,46],[41,63,47],[41,63,48],[41,63,49],[41,63,50],[41,63,51],[41,63,52],[41,63,53],[41,64,47],[41,64,48],[41,64,49],[41,64,50],[41,64,51],[41,64,52],[41,64,53],[41,65,28],[41,65,29],[41,65,45],[41,65,46],[41,65,47],[41,65,48],[41,65,49],[41,65,50],[41,65,51],[41,65,52],[41,65,53],[41,66,27],[41,66,28],[41,66,29],[41,66,45],[41,66,46],[41,66,47],[41,66,48],[41,66,49],[41,66,50],[41,66,51],[41,66,52],[41,66,53],[41,67,27],[41,67,28],[41,67,29],[41,67,45],[41,67,46],[41,67,47],[41,67,48],[41,67,49],[41,67,50],[41,67,51],[41,68,27],[41,68,28],[41,68,29],[41,68,45],[41,68,46],[41,68,47],[41,68,48],[41,69,27],[41,69,28],[41,69,29],[41,69,30],[41,69,41],[41,69,42],[41,69,43],[41,69,44],[41,69,45],[41,69,46],[41,69,47],[41,69,48],[41,70,27],[41,70,28],[41,70,29],[41,70,30],[41,70,31],[41,70,41],[41,70,42],[41,70,43],[41,70,44],[41,70,45],[41,70,46],[41,70,47],[41,71,27],[41,71,28],[41,71,29],[41,71,30],[41,71,31],[41,71,32],[41,71,37],[41,71,38],[41,71,39],[41,71,40],[41,71,41],[41,71,42],[41,71,43],[41,71,44],[41,71,45],[41,71,46],[41,71,47],[41,72,27],[41,72,28],[41,72,29],[41,72,30],[41,72,31],[41,72,32],[41,72,37],[41,72,38],[41,72,39],[41,72,40],[41,72,41],[41,72,42],[41,72,43],[41,72,44],[41,72,45],[41,72,46],[41,73,27],[41,73,28],[41,73,29],[41,73,30],[41,73,31],[41,73,32],[41,73,33],[41,73,34],[41,73,35],[41,73,36],[41,73,37],[41,73,38],[41,73,39],[41,73,40],[41,73,41],[41,73,42],[41,74,27],[41,74,28],[41,74,29],[41,74,30],[41,74,31],[41,74,32],[41,74,33],[41,74,34],[41,74,35],[41,74,36],[41,74,37],[41,74,38],[41,74,39],[41,74,40],[41,74,41],[41,74,42],[41,75,29],[41,75,30],[41,75,31],[41,75,32],[41,75,33],[41,75,34],[41,75,35],[41,75,36],[41,75,37],[41,75,38],[41,75,39],[41,75,40],[41,76,29],[41,76,30],[41,76,31],[41,76,32],[41,76,33],[41,76,34],[41,76,35],[41,76,36],[41,76,37],[41,76,38],[41,76,39],[41,76,40],[41,77,29],[41,77,30],[41,77,31],[41,77,32],[41,77,33],[41,77,34],[41,77,35],[41,77,36],[41,77,37],[41,77,38],[41,78,31],[41,78,32],[41,78,33],[41,78,34],[41,78,35],[41,78,36],[41,78,37],[41,78,38],[41,79,31],[42,57,50],[42,57,51],[42,57,52],[42,57,53],[42,57,54],[42,57,55],[42,58,50],[42,58,51],[42,58,52],[42,58,53],[42,58,54],[42,58,55],[42,58,56],[42,59,49],[42,59,50],[42,59,51],[42,59,52],[42,59,53],[42,59,54],[42,59,55],[42,59,56],[42,60,49],[42,60,50],[42,60,51],[42,60,52],[42,60,53],[42,60,54],[42,60,55],[42,60,56],[42,61,47],[42,61,48],[42,61,49],[42,61,50],[42,61,51],[42,61,52],[42,61,53],[42,62,46],[42,62,47],[42,62,48],[42,62,49],[42,62,50],[42,62,51],[42,62,52],[42,62,53],[42,63,46],[42,63,47],[42,63,48],[42,63,49],[42,63,50],[42,63,51],[42,63,52],[42,63,53],[42,64,27],[42,64,28],[42,64,29],[42,64,45],[42,64,46],[42,64,47],[42,64,48],[42,64,49],[42,64,50],[42,64,51],[42,64,52],[42,64,53],[42,65,27],[42,65,28],[42,65,29],[42,65,45],[42,65,46],[42,65,47],[42,65,48],[42,65,49],[42,65,50],[42,65,51],[42,65,52],[42,65,53],[42,66,27],[42,66,28],[42,66,29],[42,66,45],[42,66,46],[42,66,47],[42,66,48],[42,66,49],[42,66,50],[42,66,51],[42,66,52],[42,66,53],[42,67,27],[42,67,28],[42,67,29],[42,67,45],[42,67,46],[42,67,47],[42,67,48],[42,67,49],[42,67,50],[42,67,51],[42,68,27],[42,68,45],[42,68,46],[42,68,47],[42,68,48],[42,69,27],[42,69,28],[42,69,29],[42,69,30],[42,69,41],[42,69,42],[42,69,43],[42,69,44],[42,69,45],[42,69,46],[42,69,47],[42,69,48],[42,70,27],[42,70,28],[42,70,29],[42,70,30],[42,70,31],[42,70,41],[42,70,42],[42,70,43],[42,70,44],[42,70,45],[42,70,46],[42,70,47],[42,71,27],[42,71,28],[42,71,29],[42,71,30],[42,71,31],[42,71,32],[42,71,37],[42,71,38],[42,71,39],[42,71,40],[42,71,41],[42,71,42],[42,71,43],[42,71,44],[42,71,45],[42,71,46],[42,71,47],[42,72,27],[42,72,28],[42,72,29],[42,72,30],[42,72,31],[42,72,32],[42,72,37],[42,72,38],[42,72,39],[42,72,40],[42,72,41],[42,72,42],[42,72,43],[42,72,44],[42,72,45],[42,72,46],[42,73,27],[42,73,28],[42,73,29],[42,73,30],[42,73,31],[42,73,32],[42,73,33],[42,73,34],[42,73,35],[42,73,36],[42,73,37],[42,73,38],[42,73,39],[42,73,40],[42,73,41],[42,73,42],[42,73,43],[42,73,44],[42,73,45],[42,74,27],[42,74,28],[42,74,29],[42,74,30],[42,74,31],[42,74,32],[42,74,33],[42,74,34],[42,74,35],[42,74,36],[42,74,37],[42,74,38],[42,74,39],[42,74,40],[42,74,41],[42,74,42],[42,74,43],[42,75,29],[42,75,30],[42,75,31],[42,75,32],[42,75,33],[42,75,34],[42,75,35],[42,75,36],[42,75,37],[42,75,38],[42,75,39],[42,75,40],[42,76,29],[42,76,30],[42,76,31],[42,76,32],[42,76,33],[42,76,34],[42,76,35],[42,76,36],[42,76,37],[42,76,38],[42,76,39],[42,76,40],[42,77,29],[42,77,30],[42,77,31],[42,77,32],[42,77,33],[42,77,34],[42,77,35],[42,77,36],[42,77,37],[42,77,38],[42,78,29],[42,78,30],[42,78,31],[42,78,32],[42,78,33],[42,78,34],[42,78,35],[42,78,36],[42,78,37],[42,78,38],[43,57,50],[43,57,51],[43,57,52],[43,57,53],[43,57,54],[43,57,55],[43,58,50],[43,58,51],[43,58,52],[43,58,53],[43,58,54],[43,58,55],[43,58,56],[43,59,49],[43,59,50],[43,59,51],[43,59,52],[43,59,53],[43,59,54],[43,59,55],[43,59,56],[43,60,48],[43,60,49],[43,60,50],[43,60,51],[43,60,52],[43,60,53],[43,60,54],[43,60,55],[43,60,56],[43,61,47],[43,61,48],[43,61,49],[43,61,50],[43,62,46],[43,62,47],[43,62,48],[43,62,49],[43,62,50],[43,62,51],[43,62,52],[43,62,53],[43,63,45],[43,63,46],[43,63,47],[43,63,48],[43,63,50],[43,63,51],[43,63,52],[43,63,53],[43,64,27],[43,64,28],[43,64,29],[43,64,45],[43,64,46],[43,64,47],[43,64,48],[43,64,50],[43,64,51],[43,64,52],[43,64,53],[43,65,27],[43,65,28],[43,65,29],[43,65,45],[43,65,46],[43,65,47],[43,65,48],[43,65,49],[43,65,50],[43,65,51],[43,65,52],[43,65,53],[43,66,27],[43,66,28],[43,66,29],[43,66,43],[43,66,44],[43,66,45],[43,66,46],[43,66,47],[43,66,48],[43,66,49],[43,66,50],[43,66,51],[43,66,52],[43,66,53],[43,67,27],[43,67,28],[43,67,29],[43,67,43],[43,67,44],[43,67,45],[43,67,46],[43,67,47],[43,67,48],[43,67,49],[43,67,50],[43,67,51],[43,68,43],[43,68,44],[43,68,45],[43,68,46],[43,68,47],[43,68,48],[43,69,41],[43,69,42],[43,69,43],[43,69,44],[43,69,45],[43,69,46],[43,70,27],[43,70,28],[43,70,29],[43,70,30],[43,70,31],[43,70,39],[43,70,40],[43,70,41],[43,70,42],[43,70,43],[43,70,44],[43,70,45],[43,70,46],[43,70,47],[43,71,27],[43,71,28],[43,71,29],[43,71,30],[43,71,31],[43,71,37],[43,71,38],[43,71,39],[43,71,40],[43,71,41],[43,71,42],[43,71,43],[43,71,44],[43,71,45],[43,71,46],[43,71,47],[43,72,27],[43,72,28],[43,72,29],[43,72,30],[43,72,31],[43,72,32],[43,72,37],[43,72,38],[43,72,39],[43,72,40],[43,72,41],[43,72,42],[43,72,43],[43,72,44],[43,72,45],[43,72,46],[43,73,27],[43,73,28],[43,73,29],[43,73,30],[43,73,31],[43,73,32],[43,73,33],[43,73,34],[43,73,35],[43,73,36],[43,73,37],[43,73,38],[43,73,39],[43,73,40],[43,73,41],[43,73,42],[43,73,43],[43,73,44],[43,73,45],[43,74,27],[43,74,28],[43,74,29],[43,74,30],[43,74,31],[43,74,32],[43,74,33],[43,74,34],[43,74,35],[43,74,36],[43,74,37],[43,74,38],[43,74,39],[43,74,40],[43,74,41],[43,74,42],[43,74,43],[43,75,30],[43,75,31],[43,75,32],[43,75,33],[43,75,34],[43,75,35],[43,75,36],[43,75,37],[43,75,38],[43,75,39],[43,75,40],[43,76,29],[43,76,30],[43,76,31],[43,76,32],[43,76,33],[43,76,34],[43,76,35],[43,76,36],[43,76,37],[43,76,38],[43,76,39],[43,76,40],[43,77,29],[43,77,30],[43,77,31],[43,77,32],[43,77,33],[43,77,34],[43,77,35],[43,77,36],[43,77,37],[43,77,38],[43,78,29],[43,78,30],[43,78,31],[43,78,32],[43,78,33],[43,78,34],[43,78,35],[43,78,36],[44,57,50],[44,57,51],[44,57,52],[44,57,53],[44,58,50],[44,58,51],[44,58,52],[44,58,53],[44,58,54],[44,58,55],[44,58,56],[44,59,49],[44,59,50],[44,59,51],[44,59,52],[44,59,53],[44,59,54],[44,59,55],[44,59,56],[44,60,48],[44,60,49],[44,60,50],[44,60,52],[44,60,53],[44,60,54],[44,60,55],[44,60,56],[44,61,47],[44,61,48],[44,61,49],[44,61,50],[44,62,46],[44,62,47],[44,62,48],[44,62,49],[44,62,50],[44,63,45],[44,63,46],[44,63,47],[44,63,48],[44,63,50],[44,63,51],[44,63,52],[44,63,53],[44,64,27],[44,64,28],[44,64,29],[44,64,45],[44,64,46],[44,64,47],[44,64,48],[44,64,50],[44,64,51],[44,64,52],[44,64,53],[44,65,27],[44,65,28],[44,65,29],[44,65,43],[44,65,45],[44,65,46],[44,65,47],[44,65,48],[44,65,49],[44,65,50],[44,65,51],[44,65,52],[44,65,53],[44,66,27],[44,66,28],[44,66,29],[44,66,43],[44,66,44],[44,66,45],[44,66,46],[44,66,47],[44,66,48],[44,66,49],[44,66,50],[44,66,51],[44,66,52],[44,66,53],[44,67,43],[44,67,44],[44,67,45],[44,67,47],[44,67,48],[44,68,43],[44,68,44],[44,68,45],[44,68,48],[44,69,43],[44,69,44],[44,69,45],[44,69,46],[44,69,47],[44,70,27],[44,70,28],[44,70,29],[44,70,30],[44,70,39],[44,70,40],[44,70,41],[44,70,45],[44,70,46],[44,70,47],[44,71,27],[44,71,28],[44,71,29],[44,71,30],[44,71,37],[44,71,38],[44,71,39],[44,71,40],[44,71,41],[44,71,42],[44,71,43],[44,71,44],[44,71,45],[44,71,46],[44,72,27],[44,72,28],[44,72,29],[44,72,30],[44,72,37],[44,72,39],[44,72,40],[44,72,41],[44,72,42],[44,72,43],[44,72,44],[44,72,45],[44,72,46],[44,73,27],[44,73,28],[44,73,29],[44,73,37],[44,73,38],[44,73,39],[44,73,40],[44,73,41],[44,73,42],[44,73,43],[44,73,44],[44,73,45],[44,74,27],[44,74,28],[44,74,29],[44,74,32],[44,74,33],[44,74,34],[44,74,35],[44,74,36],[44,74,37],[44,74,38],[44,74,39],[44,74,40],[44,74,41],[44,74,42],[44,74,43],[44,74,44],[44,75,30],[44,75,31],[44,75,32],[44,75,33],[44,75,34],[44,75,35],[44,75,36],[44,75,37],[44,75,38],[44,75,39],[44,75,40],[44,76,29],[44,76,30],[44,76,31],[44,76,32],[44,76,33],[44,76,34],[44,76,35],[44,76,36],[44,76,37],[44,76,38],[44,76,40],[44,77,29],[44,77,30],[44,77,31],[44,77,32],[44,77,33],[44,77,34],[44,77,35],[44,77,36],[44,77,37],[44,77,38],[44,78,29],[44,78,30],[44,78,31],[44,78,32],[45,57,50],[45,57,51],[45,57,52],[45,57,53],[45,58,50],[45,58,51],[45,58,52],[45,58,53],[45,59,49],[45,59,50],[45,59,51],[45,59,52],[45,59,53],[45,59,54],[45,59,55],[45,59,56],[45,60,48],[45,60,49],[45,60,50],[45,60,53],[45,60,54],[45,60,55],[45,60,56],[45,61,47],[45,61,48],[45,61,49],[45,61,50],[45,61,53],[45,61,54],[45,61,55],[45,61,56],[45,62,46],[45,62,47],[45,62,48],[45,62,49],[45,62,50],[45,63,45],[45,63,46],[45,63,47],[45,63,48],[45,63,50],[45,63,51],[45,63,52],[45,63,53],[45,64,46],[45,64,47],[45,64,48],[45,64,50],[45,64,51],[45,64,52],[45,64,53],[45,65,43],[45,65,46],[45,65,47],[45,65,48],[45,65,49],[45,65,50],[45,65,51],[45,65,52],[45,65,53],[45,66,43],[45,66,44],[45,66,45],[45,66,47],[45,66,48],[45,66,49],[45,66,50],[45,66,51],[45,66,52],[45,66,53],[45,67,43],[45,67,44],[45,67,45],[45,67,47],[45,67,48],[45,68,43],[45,68,44],[45,68,45],[45,68,46],[45,69,45],[45,69,46],[45,69,47],[45,70,27],[45,70,28],[45,70,29],[45,70,30],[45,70,39],[45,70,40],[45,70,41],[45,70,42],[45,70,43],[45,70,45],[45,70,46],[45,70,47],[45,71,27],[45,71,28],[45,71,29],[45,71,30],[45,71,39],[45,71,40],[45,71,41],[45,71,42],[45,71,43],[45,71,44],[45,71,45],[45,71,46],[45,72,27],[45,72,28],[45,72,29],[45,72,30],[45,72,37],[45,72,38],[45,72,39],[45,72,40],[45,72,41],[45,72,42],[45,72,43],[45,72,45],[45,72,46],[45,73,37],[45,73,38],[45,73,39],[45,73,40],[45,73,41],[45,73,42],[45,74,34],[45,74,35],[45,74,36],[45,74,37],[45,74,38],[45,74,39],[45,74,40],[45,74,41],[45,75,30],[45,75,31],[45,75,32],[45,75,34],[45,75,35],[45,75,36],[45,75,37],[45,75,38],[45,75,39],[45,75,40],[45,76,29],[45,76,30],[45,76,31],[45,76,32],[45,76,34],[45,76,35],[45,76,36],[45,76,37],[45,76,38],[45,77,29],[45,77,30],[45,77,31],[45,77,32],[45,78,29],[45,78,30],[45,78,31],[45,78,32],[46,57,50],[46,57,51],[46,57,52],[46,57,53],[46,58,50],[46,58,51],[46,58,52],[46,58,53],[46,59,48],[46,59,49],[46,59,50],[46,59,51],[46,59,52],[46,59,53],[46,59,54],[46,59,55],[46,59,56],[46,60,48],[46,60,49],[46,60,50],[46,60,53],[46,60,54],[46,60,55],[46,60,56],[46,61,48],[46,61,49],[46,61,50],[46,61,53],[46,61,54],[46,61,55],[46,61,56],[46,62,45],[46,62,46],[46,62,47],[46,62,48],[46,62,49],[46,62,50],[46,63,45],[46,63,46],[46,63,47],[46,63,48],[46,63,51],[46,63,52],[46,63,53],[46,64,45],[46,64,46],[46,64,47],[46,64,48],[46,64,50],[46,64,51],[46,64,52],[46,64,53],[46,65,43],[46,65,44],[46,65,45],[46,65,46],[46,65,47],[46,65,48],[46,65,49],[46,65,50],[46,65,51],[46,65,52],[46,65,53],[46,66,43],[46,66,44],[46,66,45],[46,66,46],[46,66,47],[46,66,48],[46,66,49],[46,66,50],[46,66,51],[46,66,52],[46,66,53],[46,67,43],[46,67,44],[46,67,45],[46,67,46],[46,67,47],[46,67,48],[46,67,49],[46,68,45],[46,68,46],[46,68,47],[46,69,45],[46,69,46],[46,69,47],[46,70,27],[46,70,28],[46,70,29],[46,70,30],[46,70,41],[46,70,45],[46,70,46],[46,71,27],[46,71,28],[46,71,29],[46,71,30],[46,71,39],[46,71,40],[46,71,41],[46,71,42],[46,71,43],[46,71,44],[46,71,45],[46,72,27],[46,72,28],[46,72,29],[46,72,30],[46,72,37],[46,72,38],[46,72,39],[46,72,40],[46,72,41],[46,72,42],[46,72,43],[46,72,44],[46,73,37],[46,73,38],[46,73,39],[46,73,40],[46,73,41],[46,73,42],[46,74,34],[46,74,35],[46,74,36],[46,74,37],[46,74,38],[46,74,39],[46,74,40],[46,75,29],[46,75,32],[46,75,34],[46,75,35],[46,75,36],[46,75,37],[46,75,38],[46,76,29],[46,76,30],[46,76,31],[46,76,32],[46,76,34],[46,76,35],[46,76,36],[46,76,37],[46,76,38],[46,77,29],[46,77,30],[46,77,31],[46,77,32],[46,78,29],[46,78,30],[46,78,31],[46,78,32],[46,79,30],[46,79,31],[47,57,50],[47,57,51],[47,57,52],[47,57,53],[47,58,50],[47,58,51],[47,58,52],[47,58,53],[47,59,48],[47,59,49],[47,59,50],[47,59,51],[47,59,52],[47,59,53],[47,59,54],[47,59,55],[47,59,56],[47,60,48],[47,60,49],[47,60,50],[47,60,53],[47,60,54],[47,60,55],[47,60,56],[47,61,49],[47,61,50],[47,61,53],[47,61,54],[47,61,55],[47,61,56],[47,62,47],[47,62,48],[47,62,49],[47,62,50],[47,63,47],[47,63,48],[47,64,47],[47,64,48],[47,65,47],[47,65,48],[47,65,49],[47,65,50],[47,66,46],[47,66,47],[47,66,48],[47,66,49],[47,66,50],[47,66,51],[47,67,45],[47,67,46],[47,67,47],[47,67,48],[47,67,49],[47,67,50],[47,67,51],[47,68,45],[47,68,46],[47,68,47],[47,69,41],[47,69,42],[47,69,43],[47,69,45],[47,69,46],[47,69,47],[47,70,41],[47,70,42],[47,70,43],[47,70,44],[47,70,45],[47,71,39],[47,71,40],[47,71,41],[47,71,42],[47,71,43],[47,71,44],[47,72,37],[47,72,38],[47,72,39],[47,72,40],[47,72,41],[47,72,42],[47,72,43],[47,72,44],[47,72,45],[47,73,37],[47,73,38],[47,73,39],[47,73,40],[47,74,34],[47,74,35],[47,74,36],[47,74,37],[47,74,38],[47,74,39],[47,74,40],[47,75,29],[47,75,32],[47,75,34],[47,75,35],[47,75,36],[47,75,37],[47,75,38],[47,76,29],[47,76,30],[47,76,31],[47,76,32],[47,76,34],[47,76,35],[47,76,36],[47,76,37],[47,76,38],[47,77,29],[47,77,30],[47,77,31],[47,77,32],[47,77,36],[47,77,37],[47,77,38],[47,78,30],[47,78,31],[48,57,50],[48,57,51],[48,57,52],[48,58,53],[48,58,54],[48,58,55],[48,58,56],[48,59,48],[48,59,49],[48,59,50],[48,59,53],[48,59,54],[48,59,55],[48,59,56],[48,60,48],[48,60,49],[48,60,50],[48,60,53],[48,60,54],[48,60,55],[48,60,56],[48,61,48],[48,61,49],[48,61,50],[48,61,53],[48,61,54],[48,61,55],[48,62,48],[48,62,49],[48,62,50],[48,63,48],[48,64,47],[48,64,48],[48,65,48],[48,65,49],[48,65,50],[48,66,47],[48,66,48],[48,66,49],[48,66,50],[48,66,51],[48,67,47],[48,67,48],[48,67,49],[48,67,50],[48,67,51],[48,68,45],[48,68,46],[48,69,45],[48,69,46],[48,70,41],[48,70,42],[48,70,43],[48,70,44],[48,70,45],[48,70,46],[48,71,39],[48,71,40],[48,71,41],[48,71,42],[48,71,43],[48,71,44],[48,71,45],[48,72,38],[48,72,39],[48,72,40],[48,72,41],[48,72,42],[48,72,43],[48,72,44],[48,72,45],[48,73,37],[48,73,38],[48,73,39],[48,73,40],[48,73,41],[48,74,34],[48,74,35],[48,74,36],[48,74,37],[48,74,38],[48,74,39],[48,74,40],[48,75,29],[48,75,30],[48,75,31],[48,75,32],[48,75,34],[48,75,35],[48,75,36],[48,75,37],[48,75,38],[48,76,29],[48,76,30],[48,76,31],[48,76,34],[48,76,35],[48,76,36],[48,76,37],[48,76,38],[48,77,30],[48,77,31],[48,77,36],[48,77,37],[48,77,38],[49,58,53],[49,58,54],[49,58,55],[49,58,56],[49,59,53],[49,59,54],[49,59,55],[49,59,56],[49,60,53],[49,60,54],[49,60,55],[49,60,56],[49,70,41],[49,70,42],[49,70,43],[49,70,44],[49,70,45],[49,71,41],[49,71,42],[49,71,43],[49,71,44],[49,71,45],[49,72,39],[49,72,40],[49,72,41],[49,72,42],[49,72,43],[49,72,44],[49,72,45],[49,73,37],[49,73,38],[49,73,39],[49,73,40],[49,73,41],[49,74,38],[49,74,39],[49,74,40],[49,75,36],[49,75,37],[49,75,38],[49,76,36],[49,76,37],[49,76,38],[49,77,36],[49,77,37],[49,77,38],[50,58,53],[50,58,54],[50,58,55],[50,59,53],[50,59,54],[50,59,55],[50,60,53],[50,60,54],[50,60,55],[50,72,39],[50,72,40],[50,72,41],[50,72,43],[50,73,38],[50,73,39],[50,73,40],[50,73,41],[50,74,38],[50,74,39],[50,74,40]],"ignoreExtent":false,"flags":4096},"176":{"id":176,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,68,43],[39,69,44]],"colors":[[1,0,0,1]],"centers":[[38,68,43],[39,69,44]],"ignoreExtent":false,"flags":16448},"177":{"id":177,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,72,40],[32,71,39]],"colors":[[1,0,0,1]],"centers":[[31,72,40],[32,71,39]],"ignoreExtent":false,"flags":16448},"178":{"id":178,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,44],[37,66,45]],"colors":[[1,0,0,1]],"centers":[[36,67,44],[37,66,45]],"ignoreExtent":false,"flags":16448},"179":{"id":179,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,41],[37,73,42]],"colors":[[1,0,0,1]],"centers":[[36,74,41],[37,73,42]],"ignoreExtent":false,"flags":16448},"180":{"id":180,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,70,46],[39,71,45]],"colors":[[1,0,0,1]],"centers":[[38,70,46],[39,71,45]],"ignoreExtent":false,"flags":16448},"181":{"id":181,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[35,66,44]],"colors":[[1,0,0,1]],"centers":[[34,67,45],[35,66,44]],"ignoreExtent":false,"flags":16448},"182":{"id":182,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,40],[32,70,41]],"colors":[[1,0,0,1]],"centers":[[31,71,40],[32,70,41]],"ignoreExtent":false,"flags":16448},"183":{"id":183,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,38],[34,71,39]],"colors":[[1,0,0,1]],"centers":[[33,72,38],[34,71,39]],"ignoreExtent":false,"flags":16448},"184":{"id":184,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,44],[33,69,45]],"colors":[[1,0,0,1]],"centers":[[32,70,44],[33,69,45]],"ignoreExtent":false,"flags":16448},"185":{"id":185,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,75,39]],"colors":[[1,0,0,1]],"centers":[[36,74,40],[36,75,39]],"ignoreExtent":false,"flags":16448},"186":{"id":186,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,38]],"colors":[[1,0,0,1]],"centers":[[34,71,39],[35,71,38]],"ignoreExtent":false,"flags":16448},"187":{"id":187,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,45],[39,70,44]],"colors":[[1,0,0,1]],"centers":[[39,69,45],[39,70,44]],"ignoreExtent":false,"flags":16448},"188":{"id":188,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,40],[31,72,39]],"colors":[[1,0,0,1]],"centers":[[31,71,40],[31,72,39]],"ignoreExtent":false,"flags":16448},"189":{"id":189,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,74,36],[35,75,37]],"colors":[[1,0,0,1]],"centers":[[35,74,36],[35,75,37]],"ignoreExtent":false,"flags":16448},"190":{"id":190,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,65,44],[36,66,43]],"colors":[[1,0,0,1]],"centers":[[36,65,44],[36,66,43]],"ignoreExtent":false,"flags":16448},"191":{"id":191,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,67,43],[36,66,43]],"colors":[[1,0,0,1]],"centers":[[35,67,43],[36,66,43]],"ignoreExtent":false,"flags":16448},"192":{"id":192,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[36,68,43]],"colors":[[1,0,0,1]],"centers":[[36,67,43],[36,68,43]],"ignoreExtent":false,"flags":16448},"193":{"id":193,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,70,44],[39,71,45]],"colors":[[1,0,0,1]],"centers":[[39,70,44],[39,71,45]],"ignoreExtent":false,"flags":16448},"194":{"id":194,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,65,45],[37,66,45]],"colors":[[1,0,0,1]],"centers":[[36,65,45],[37,66,45]],"ignoreExtent":false,"flags":16448},"195":{"id":195,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,39],[31,72,39]],"colors":[[1,0,0,1]],"centers":[[31,71,39],[31,72,39]],"ignoreExtent":false,"flags":16448},"196":{"id":196,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,66,44],[35,67,43]],"colors":[[1,0,0,1]],"centers":[[35,66,44],[35,67,43]],"ignoreExtent":false,"flags":16448},"197":{"id":197,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[37,71,45]],"colors":[[1,0,0,1]],"centers":[[37,70,46],[37,71,45]],"ignoreExtent":false,"flags":16448},"198":{"id":198,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,44],[33,70,43]],"colors":[[1,0,0,1]],"centers":[[32,70,44],[33,70,43]],"ignoreExtent":false,"flags":16448},"199":{"id":199,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,40],[33,72,40]],"colors":[[1,0,0,1]],"centers":[[32,71,40],[33,72,40]],"ignoreExtent":false,"flags":16448},"200":{"id":200,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,66,43],[37,67,43]],"colors":[[1,0,0,1]],"centers":[[36,66,43],[37,67,43]],"ignoreExtent":false,"flags":16448},"201":{"id":201,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[33,72,40]],"colors":[[1,0,0,1]],"centers":[[33,71,39],[33,72,40]],"ignoreExtent":false,"flags":16448},"202":{"id":202,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,68,43],[37,68,43]],"colors":[[1,0,0,1]],"centers":[[36,68,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"203":{"id":203,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,43],[34,70,42]],"colors":[[1,0,0,1]],"centers":[[33,70,43],[34,70,42]],"ignoreExtent":false,"flags":16448},"204":{"id":204,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,72,44],[37,73,43]],"colors":[[1,0,0,1]],"centers":[[37,72,44],[37,73,43]],"ignoreExtent":false,"flags":16448},"205":{"id":205,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,40],[31,72,41]],"colors":[[1,0,0,1]],"centers":[[31,71,40],[31,72,41]],"ignoreExtent":false,"flags":16448},"206":{"id":206,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[37,68,43]],"colors":[[1,0,0,1]],"centers":[[36,67,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"207":{"id":207,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[35,74,36]],"colors":[[1,0,0,1]],"centers":[[34,74,37],[35,74,36]],"ignoreExtent":false,"flags":16448},"208":{"id":208,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[37,72,44]],"colors":[[1,0,0,1]],"centers":[[37,71,45],[37,72,44]],"ignoreExtent":false,"flags":16448},"209":{"id":209,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,74,37]],"colors":[[1,0,0,1]],"centers":[[33,73,37],[34,74,37]],"ignoreExtent":false,"flags":16448},"210":{"id":210,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[34,67,45]],"colors":[[1,0,0,1]],"centers":[[33,67,45],[34,67,45]],"ignoreExtent":false,"flags":16448},"211":{"id":211,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,37],[35,75,38]],"colors":[[1,0,0,1]],"centers":[[35,75,37],[35,75,38]],"ignoreExtent":false,"flags":16448},"212":{"id":212,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,43],[37,68,43]],"colors":[[1,0,0,1]],"centers":[[37,67,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"213":{"id":213,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[36,67,44]],"colors":[[1,0,0,1]],"centers":[[36,67,43],[36,67,44]],"ignoreExtent":false,"flags":16448},"214":{"id":214,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,38],[35,75,39]],"colors":[[1,0,0,1]],"centers":[[35,75,38],[35,75,39]],"ignoreExtent":false,"flags":16448},"215":{"id":215,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,39],[32,71,40]],"colors":[[1,0,0,1]],"centers":[[32,71,39],[32,71,40]],"ignoreExtent":false,"flags":16448},"216":{"id":216,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[38,70,46]],"colors":[[1,0,0,1]],"centers":[[37,70,46],[38,70,46]],"ignoreExtent":false,"flags":16448},"217":{"id":217,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,41],[34,70,41]],"colors":[[1,0,0,1]],"centers":[[33,70,41],[34,70,41]],"ignoreExtent":false,"flags":16448},"218":{"id":218,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[33,73,37]],"colors":[[1,0,0,1]],"centers":[[33,72,37],[33,73,37]],"ignoreExtent":false,"flags":16448},"219":{"id":219,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,66,43],[36,67,43]],"colors":[[1,0,0,1]],"centers":[[36,66,43],[36,67,43]],"ignoreExtent":false,"flags":16448},"220":{"id":220,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,71,38],[35,71,39]],"colors":[[1,0,0,1]],"centers":[[35,71,38],[35,71,39]],"ignoreExtent":false,"flags":16448},"221":{"id":221,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,39],[31,71,40]],"colors":[[1,0,0,1]],"centers":[[31,71,39],[31,71,40]],"ignoreExtent":false,"flags":16448},"222":{"id":222,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[33,68,45]],"colors":[[1,0,0,1]],"centers":[[33,67,45],[33,68,45]],"ignoreExtent":false,"flags":16448},"223":{"id":223,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,39]],"colors":[[1,0,0,1]],"centers":[[34,71,39],[35,71,39]],"ignoreExtent":false,"flags":16448},"224":{"id":224,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,39],[36,75,39]],"colors":[[1,0,0,1]],"centers":[[35,75,39],[36,75,39]],"ignoreExtent":false,"flags":16448},"225":{"id":225,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[33,69,45]],"colors":[[1,0,0,1]],"centers":[[33,68,45],[33,69,45]],"ignoreExtent":false,"flags":16448},"226":{"id":226,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,41],[34,70,42]],"colors":[[1,0,0,1]],"centers":[[34,70,41],[34,70,42]],"ignoreExtent":false,"flags":16448},"227":{"id":227,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[38,68,43]],"colors":[[1,0,0,1]],"centers":[[37,68,43],[38,68,43]],"ignoreExtent":false,"flags":16448},"228":{"id":228,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,74,41]],"colors":[[1,0,0,1]],"centers":[[36,74,40],[36,74,41]],"ignoreExtent":false,"flags":16448},"229":{"id":229,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[33,72,38]],"colors":[[1,0,0,1]],"centers":[[33,72,37],[33,72,38]],"ignoreExtent":false,"flags":16448},"230":{"id":230,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,41],[33,70,41]],"colors":[[1,0,0,1]],"centers":[[32,70,41],[33,70,41]],"ignoreExtent":false,"flags":16448},"231":{"id":231,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,44],[39,69,45]],"colors":[[1,0,0,1]],"centers":[[39,69,44],[39,69,45]],"ignoreExtent":false,"flags":16448},"232":{"id":232,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,65,44],[36,65,45]],"colors":[[1,0,0,1]],"centers":[[36,65,44],[36,65,45]],"ignoreExtent":false,"flags":16448},"233":{"id":233,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[34,71,39]],"colors":[[1,0,0,1]],"centers":[[33,71,39],[34,71,39]],"ignoreExtent":false,"flags":16448},"234":{"id":234,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,73,43]],"colors":[[1,0,0,1]],"centers":[[37,73,42],[37,73,43]],"ignoreExtent":false,"flags":16448},"235":{"id":235,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,72,40],[31,72,41]],"colors":[[1,0,0,1]],"centers":[[31,72,40],[31,72,41]],"ignoreExtent":false,"flags":16448},"236":{"id":236,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,68,43],[39,69,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[38,68,43],[39,69,44]],"ignoreExtent":false,"flags":16448},"237":{"id":237,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,71,45],[39,70,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[38,71,45],[39,70,44]],"ignoreExtent":false,"flags":16448},"238":{"id":238,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,44],[38,68,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,67,44],[38,68,43]],"ignoreExtent":false,"flags":16448},"239":{"id":239,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,43],[33,70,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[32,70,43],[33,70,43]],"ignoreExtent":false,"flags":16448},"240":{"id":240,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,40],[35,70,41]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,71,40],[35,70,41]],"ignoreExtent":false,"flags":16448},"241":{"id":241,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,68,45],[35,67,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,68,45],[35,67,44]],"ignoreExtent":false,"flags":16448},"242":{"id":242,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,44],[39,70,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[39,69,44],[39,70,44]],"ignoreExtent":false,"flags":16448},"243":{"id":243,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,40],[35,71,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,71,40],[35,71,39]],"ignoreExtent":false,"flags":16448},"244":{"id":244,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,74,41]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,73,42],[37,74,41]],"ignoreExtent":false,"flags":16448},"245":{"id":245,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,39],[36,75,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[35,75,39],[36,75,39]],"ignoreExtent":false,"flags":16448},"246":{"id":246,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,42],[33,70,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,70,42],[33,70,43]],"ignoreExtent":false,"flags":16448},"247":{"id":247,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[34,67,45]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,68,45],[34,67,45]],"ignoreExtent":false,"flags":16448},"248":{"id":248,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,44],[33,70,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[32,70,44],[33,70,44]],"ignoreExtent":false,"flags":16448},"249":{"id":249,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,67,44],[35,68,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[35,67,44],[35,68,43]],"ignoreExtent":false,"flags":16448},"250":{"id":250,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,74,37]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,73,37],[34,74,37]],"ignoreExtent":false,"flags":16448},"251":{"id":251,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,75,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[36,74,40],[36,75,39]],"ignoreExtent":false,"flags":16448},"252":{"id":252,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,72,44],[37,73,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,72,44],[37,73,43]],"ignoreExtent":false,"flags":16448},"253":{"id":253,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[37,72,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,71,45],[37,72,44]],"ignoreExtent":false,"flags":16448},"254":{"id":254,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,72,38],[33,72,37]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[32,72,38],[33,72,37]],"ignoreExtent":false,"flags":16448},"255":{"id":255,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,74,36],[35,75,37]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[35,74,36],[35,75,37]],"ignoreExtent":false,"flags":16448},"256":{"id":256,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,69,45],[33,70,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,69,45],[33,70,44]],"ignoreExtent":false,"flags":16448},"257":{"id":257,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[35,74,36]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,74,37],[35,74,36]],"ignoreExtent":false,"flags":16448},"258":{"id":258,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,72,37]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,73,37],[34,72,37]],"ignoreExtent":false,"flags":16448},"259":{"id":259,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,72,38],[33,72,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[32,72,38],[33,72,39]],"ignoreExtent":false,"flags":16448},"260":{"id":260,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,43],[37,67,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,67,43],[37,67,44]],"ignoreExtent":false,"flags":16448},"261":{"id":261,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,68,43],[36,68,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[35,68,43],[36,68,43]],"ignoreExtent":false,"flags":16448},"262":{"id":262,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,74,40],[37,74,41]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,74,40],[37,74,41]],"ignoreExtent":false,"flags":16448},"263":{"id":263,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,41],[35,70,41]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,70,41],[35,70,41]],"ignoreExtent":false,"flags":16448},"264":{"id":264,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[34,72,37]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,72,37],[34,72,37]],"ignoreExtent":false,"flags":16448},"265":{"id":265,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[34,71,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,71,39],[34,71,39]],"ignoreExtent":false,"flags":16448},"266":{"id":266,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,43],[37,68,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,67,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"267":{"id":267,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[37,74,40]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[36,74,40],[37,74,40]],"ignoreExtent":false,"flags":16448},"268":{"id":268,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[38,71,45]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,71,45],[38,71,45]],"ignoreExtent":false,"flags":16448},"269":{"id":269,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,41],[34,70,42]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,70,41],[34,70,42]],"ignoreExtent":false,"flags":16448},"270":{"id":270,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,73,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[37,73,42],[37,73,43]],"ignoreExtent":false,"flags":16448},"271":{"id":271,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[33,69,45]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,68,45],[33,69,45]],"ignoreExtent":false,"flags":16448},"272":{"id":272,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,68,43],[37,68,43]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[36,68,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"273":{"id":273,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,42],[34,70,42]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,70,42],[34,70,42]],"ignoreExtent":false,"flags":16448},"274":{"id":274,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,43],[32,70,44]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[32,70,43],[32,70,44]],"ignoreExtent":false,"flags":16448},"275":{"id":275,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[34,68,45]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,67,45],[34,68,45]],"ignoreExtent":false,"flags":16448},"276":{"id":276,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[33,72,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[33,71,39],[33,72,39]],"ignoreExtent":false,"flags":16448},"277":{"id":277,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,37],[35,75,38]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[35,75,37],[35,75,38]],"ignoreExtent":false,"flags":16448},"278":{"id":278,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[34,71,39],[35,71,39]],"ignoreExtent":false,"flags":16448},"279":{"id":279,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,38],[35,75,39]],"colors":[[1,0.7529412,0.7960784,1]],"centers":[[35,75,38],[35,75,39]],"ignoreExtent":false,"flags":16448},"280":{"id":280,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,68,43],[39,69,44]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[38,68,43],[39,69,44]],"ignoreExtent":false,"flags":16448},"281":{"id":281,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,76,38],[35,75,37]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[34,76,38],[35,75,37]],"ignoreExtent":false,"flags":16448},"282":{"id":282,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[38,67,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,68,43],[38,67,43]],"ignoreExtent":false,"flags":16448},"283":{"id":283,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,73,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,73,42],[37,73,43]],"ignoreExtent":false,"flags":16448},"284":{"id":284,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,44],[34,70,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,70,44],[34,70,43]],"ignoreExtent":false,"flags":16448},"285":{"id":285,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,67,44],[35,68,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[35,67,44],[35,68,43]],"ignoreExtent":false,"flags":16448},"286":{"id":286,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,42],[34,70,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,70,42],[34,70,43]],"ignoreExtent":false,"flags":16448},"287":{"id":287,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,69,46],[39,69,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[38,69,46],[39,69,45]],"ignoreExtent":false,"flags":16448},"288":{"id":288,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,44],[39,69,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[39,69,44],[39,69,45]],"ignoreExtent":false,"flags":16448},"289":{"id":289,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,72,44],[38,72,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,72,44],[38,72,43]],"ignoreExtent":false,"flags":16448},"290":{"id":290,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,74,41]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,73,42],[37,74,41]],"ignoreExtent":false,"flags":16448},"291":{"id":291,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,68,43],[37,67,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[36,68,43],[37,67,43]],"ignoreExtent":false,"flags":16448},"292":{"id":292,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,43],[38,72,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,73,43],[38,72,43]],"ignoreExtent":false,"flags":16448},"293":{"id":293,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,74,37]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,73,37],[34,74,37]],"ignoreExtent":false,"flags":16448},"294":{"id":294,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,68,43],[36,68,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[35,68,43],[36,68,43]],"ignoreExtent":false,"flags":16448},"295":{"id":295,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,39],[32,72,38]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[32,71,39],[32,72,38]],"ignoreExtent":false,"flags":16448},"296":{"id":296,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,75,39]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[36,74,40],[36,75,39]],"ignoreExtent":false,"flags":16448},"297":{"id":297,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[36,67,44]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[36,67,43],[36,67,44]],"ignoreExtent":false,"flags":16448},"298":{"id":298,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,75,37],[34,76,38]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[34,75,37],[34,76,38]],"ignoreExtent":false,"flags":16448},"299":{"id":299,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[35,67,44]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[34,67,45],[35,67,44]],"ignoreExtent":false,"flags":16448},"300":{"id":300,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[37,71,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,70,46],[37,71,45]],"ignoreExtent":false,"flags":16448},"301":{"id":301,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,41],[33,71,40]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,70,41],[33,71,40]],"ignoreExtent":false,"flags":16448},"302":{"id":302,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,44],[38,67,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,67,44],[38,67,43]],"ignoreExtent":false,"flags":16448},"303":{"id":303,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[37,72,44]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,71,45],[37,72,44]],"ignoreExtent":false,"flags":16448},"304":{"id":304,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,38],[35,75,39]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[35,75,38],[35,75,39]],"ignoreExtent":false,"flags":16448},"305":{"id":305,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[33,72,38]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,72,37],[33,72,38]],"ignoreExtent":false,"flags":16448},"306":{"id":306,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,44],[37,67,44]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[36,67,44],[37,67,44]],"ignoreExtent":false,"flags":16448},"307":{"id":307,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,41],[33,70,42]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,70,41],[33,70,42]],"ignoreExtent":false,"flags":16448},"308":{"id":308,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,39],[36,75,39]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[35,75,39],[36,75,39]],"ignoreExtent":false,"flags":16448},"309":{"id":309,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,41],[37,74,41]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[36,74,41],[37,74,41]],"ignoreExtent":false,"flags":16448},"310":{"id":310,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[34,75,37]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[34,74,37],[34,75,37]],"ignoreExtent":false,"flags":16448},"311":{"id":311,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,44],[33,70,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,70,44],[33,70,45]],"ignoreExtent":false,"flags":16448},"312":{"id":312,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[34,68,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[34,67,45],[34,68,45]],"ignoreExtent":false,"flags":16448},"313":{"id":313,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[33,69,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,68,45],[33,69,45]],"ignoreExtent":false,"flags":16448},"314":{"id":314,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,74,41]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[36,74,40],[36,74,41]],"ignoreExtent":false,"flags":16448},"315":{"id":315,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,39],[33,71,39]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[32,71,39],[33,71,39]],"ignoreExtent":false,"flags":16448},"316":{"id":316,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,69,45],[33,70,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,69,45],[33,70,45]],"ignoreExtent":false,"flags":16448},"317":{"id":317,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,37],[35,75,38]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[35,75,37],[35,75,38]],"ignoreExtent":false,"flags":16448},"318":{"id":318,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[37,67,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[36,67,43],[37,67,43]],"ignoreExtent":false,"flags":16448},"319":{"id":319,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[34,68,45]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,68,45],[34,68,45]],"ignoreExtent":false,"flags":16448},"320":{"id":320,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[33,71,40]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,71,39],[33,71,40]],"ignoreExtent":false,"flags":16448},"321":{"id":321,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,69,46],[38,70,46]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[38,69,46],[38,70,46]],"ignoreExtent":false,"flags":16448},"322":{"id":322,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[38,68,43]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,68,43],[38,68,43]],"ignoreExtent":false,"flags":16448},"323":{"id":323,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,72,38],[33,72,38]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[32,72,38],[33,72,38]],"ignoreExtent":false,"flags":16448},"324":{"id":324,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[38,70,46]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[37,70,46],[38,70,46]],"ignoreExtent":false,"flags":16448},"325":{"id":325,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[33,73,37]],"colors":[[0.627451,0.1254902,0.9411765,1]],"centers":[[33,72,37],[33,73,37]],"ignoreExtent":false,"flags":16448},"326":{"id":326,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,68,43],[39,69,44]],"colors":[[0,1,0,1]],"centers":[[38,68,43],[39,69,44]],"ignoreExtent":false,"flags":16448},"327":{"id":327,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,74,35],[36,74,36]],"colors":[[0,1,0,1]],"centers":[[35,74,35],[36,74,36]],"ignoreExtent":false,"flags":16448},"328":{"id":328,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,66,43],[35,67,44]],"colors":[[0,1,0,1]],"centers":[[35,66,43],[35,67,44]],"ignoreExtent":false,"flags":16448},"329":{"id":329,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[35,74,36]],"colors":[[0,1,0,1]],"centers":[[34,74,37],[35,74,36]],"ignoreExtent":false,"flags":16448},"330":{"id":330,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,74,37]],"colors":[[0,1,0,1]],"centers":[[33,73,37],[34,74,37]],"ignoreExtent":false,"flags":16448},"331":{"id":331,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,44],[33,70,44]],"colors":[[0,1,0,1]],"centers":[[32,70,44],[33,70,44]],"ignoreExtent":false,"flags":16448},"332":{"id":332,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,40],[35,71,39]],"colors":[[0,1,0,1]],"centers":[[34,71,40],[35,71,39]],"ignoreExtent":false,"flags":16448},"333":{"id":333,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,74,36],[36,74,36]],"colors":[[0,1,0,1]],"centers":[[35,74,36],[36,74,36]],"ignoreExtent":false,"flags":16448},"334":{"id":334,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,39],[33,72,39]],"colors":[[0,1,0,1]],"centers":[[32,71,39],[33,72,39]],"ignoreExtent":false,"flags":16448},"335":{"id":335,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,66,45],[37,67,44]],"colors":[[0,1,0,1]],"centers":[[37,66,45],[37,67,44]],"ignoreExtent":false,"flags":16448},"336":{"id":336,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,44],[38,67,43]],"colors":[[0,1,0,1]],"centers":[[37,67,44],[38,67,43]],"ignoreExtent":false,"flags":16448},"337":{"id":337,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,69,46],[39,69,45]],"colors":[[0,1,0,1]],"centers":[[38,69,46],[39,69,45]],"ignoreExtent":false,"flags":16448},"338":{"id":338,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,72,44],[37,73,43]],"colors":[[0,1,0,1]],"centers":[[37,72,44],[37,73,43]],"ignoreExtent":false,"flags":16448},"339":{"id":339,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[37,71,45]],"colors":[[0,1,0,1]],"centers":[[37,70,46],[37,71,45]],"ignoreExtent":false,"flags":16448},"340":{"id":340,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[34,67,45]],"colors":[[0,1,0,1]],"centers":[[33,67,45],[34,67,45]],"ignoreExtent":false,"flags":16448},"341":{"id":341,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,75,37],[35,75,38]],"colors":[[0,1,0,1]],"centers":[[34,75,37],[35,75,38]],"ignoreExtent":false,"flags":16448},"342":{"id":342,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,72,38],[32,73,37]],"colors":[[0,1,0,1]],"centers":[[32,72,38],[32,73,37]],"ignoreExtent":false,"flags":16448},"343":{"id":343,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,69,45],[33,70,44]],"colors":[[0,1,0,1]],"centers":[[33,69,45],[33,70,44]],"ignoreExtent":false,"flags":16448},"344":{"id":344,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,75,39]],"colors":[[0,1,0,1]],"centers":[[36,74,40],[36,75,39]],"ignoreExtent":false,"flags":16448},"345":{"id":345,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[38,69,46]],"colors":[[0,1,0,1]],"centers":[[37,70,46],[38,69,46]],"ignoreExtent":false,"flags":16448},"346":{"id":346,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,68,45],[35,68,44]],"colors":[[0,1,0,1]],"centers":[[34,68,45],[35,68,44]],"ignoreExtent":false,"flags":16448},"347":{"id":347,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[38,67,43]],"colors":[[0,1,0,1]],"centers":[[37,68,43],[38,67,43]],"ignoreExtent":false,"flags":16448},"348":{"id":348,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,66,44],[37,67,43]],"colors":[[0,1,0,1]],"centers":[[37,66,44],[37,67,43]],"ignoreExtent":false,"flags":16448},"349":{"id":349,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[37,72,44]],"colors":[[0,1,0,1]],"centers":[[37,71,45],[37,72,44]],"ignoreExtent":false,"flags":16448},"350":{"id":350,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[37,68,43]],"colors":[[0,1,0,1]],"centers":[[36,67,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"351":{"id":351,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,43],[33,70,42]],"colors":[[0,1,0,1]],"centers":[[32,70,43],[33,70,42]],"ignoreExtent":false,"flags":16448},"352":{"id":352,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,72,37]],"colors":[[0,1,0,1]],"centers":[[33,73,37],[34,72,37]],"ignoreExtent":false,"flags":16448},"353":{"id":353,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,39],[32,71,40]],"colors":[[0,1,0,1]],"centers":[[32,71,39],[32,71,40]],"ignoreExtent":false,"flags":16448},"354":{"id":354,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,42],[34,70,43]],"colors":[[0,1,0,1]],"centers":[[33,70,42],[34,70,43]],"ignoreExtent":false,"flags":16448},"355":{"id":355,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,41],[32,71,40]],"colors":[[0,1,0,1]],"centers":[[32,70,41],[32,71,40]],"ignoreExtent":false,"flags":16448},"356":{"id":356,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,73,37],[33,72,37]],"colors":[[0,1,0,1]],"centers":[[32,73,37],[33,72,37]],"ignoreExtent":false,"flags":16448},"357":{"id":357,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,74,41]],"colors":[[0,1,0,1]],"centers":[[37,73,42],[37,74,41]],"ignoreExtent":false,"flags":16448},"358":{"id":358,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,66,44],[37,67,44]],"colors":[[0,1,0,1]],"centers":[[36,66,44],[37,67,44]],"ignoreExtent":false,"flags":16448},"359":{"id":359,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,36],[35,74,35]],"colors":[[0,1,0,1]],"centers":[[34,74,36],[35,74,35]],"ignoreExtent":false,"flags":16448},"360":{"id":360,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,41],[34,70,41]],"colors":[[0,1,0,1]],"centers":[[33,71,41],[34,70,41]],"ignoreExtent":false,"flags":16448},"361":{"id":361,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,72,38],[33,72,38]],"colors":[[0,1,0,1]],"centers":[[32,72,38],[33,72,38]],"ignoreExtent":false,"flags":16448},"362":{"id":362,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[38,68,43]],"colors":[[0,1,0,1]],"centers":[[37,68,43],[38,68,43]],"ignoreExtent":false,"flags":16448},"363":{"id":363,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,66,43],[36,66,43]],"colors":[[0,1,0,1]],"centers":[[35,66,43],[36,66,43]],"ignoreExtent":false,"flags":16448},"364":{"id":364,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,39]],"colors":[[0,1,0,1]],"centers":[[34,71,39],[35,71,39]],"ignoreExtent":false,"flags":16448},"365":{"id":365,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,66,43],[36,66,44]],"colors":[[0,1,0,1]],"centers":[[36,66,43],[36,66,44]],"ignoreExtent":false,"flags":16448},"366":{"id":366,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,41],[33,70,41]],"colors":[[0,1,0,1]],"centers":[[32,70,41],[33,70,41]],"ignoreExtent":false,"flags":16448},"367":{"id":367,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,43],[32,70,44]],"colors":[[0,1,0,1]],"centers":[[32,70,43],[32,70,44]],"ignoreExtent":false,"flags":16448},"368":{"id":368,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,44],[39,69,45]],"colors":[[0,1,0,1]],"centers":[[39,69,44],[39,69,45]],"ignoreExtent":false,"flags":16448},"369":{"id":369,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[34,68,45]],"colors":[[0,1,0,1]],"centers":[[34,67,45],[34,68,45]],"ignoreExtent":false,"flags":16448},"370":{"id":370,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,41],[33,71,41]],"colors":[[0,1,0,1]],"centers":[[33,70,41],[33,71,41]],"ignoreExtent":false,"flags":16448},"371":{"id":371,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,66,44],[37,66,45]],"colors":[[0,1,0,1]],"centers":[[37,66,44],[37,66,45]],"ignoreExtent":false,"flags":16448},"372":{"id":372,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,41],[34,70,42]],"colors":[[0,1,0,1]],"centers":[[34,70,41],[34,70,42]],"ignoreExtent":false,"flags":16448},"373":{"id":373,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,36],[34,74,37]],"colors":[[0,1,0,1]],"centers":[[34,74,36],[34,74,37]],"ignoreExtent":false,"flags":16448},"374":{"id":374,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,67,44],[35,68,44]],"colors":[[0,1,0,1]],"centers":[[35,67,44],[35,68,44]],"ignoreExtent":false,"flags":16448},"375":{"id":375,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,40],[34,71,40]],"colors":[[0,1,0,1]],"centers":[[33,71,40],[34,71,40]],"ignoreExtent":false,"flags":16448},"376":{"id":376,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,39],[36,75,39]],"colors":[[0,1,0,1]],"centers":[[35,75,39],[36,75,39]],"ignoreExtent":false,"flags":16448},"377":{"id":377,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,74,41]],"colors":[[0,1,0,1]],"centers":[[36,74,40],[36,74,41]],"ignoreExtent":false,"flags":16448},"378":{"id":378,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[37,67,43]],"colors":[[0,1,0,1]],"centers":[[36,67,43],[37,67,43]],"ignoreExtent":false,"flags":16448},"379":{"id":379,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,43],[37,67,44]],"colors":[[0,1,0,1]],"centers":[[37,67,43],[37,67,44]],"ignoreExtent":false,"flags":16448},"380":{"id":380,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[33,69,45]],"colors":[[0,1,0,1]],"centers":[[33,68,45],[33,69,45]],"ignoreExtent":false,"flags":16448},"381":{"id":381,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,38],[35,75,39]],"colors":[[0,1,0,1]],"centers":[[35,75,38],[35,75,39]],"ignoreExtent":false,"flags":16448},"382":{"id":382,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,41],[37,74,41]],"colors":[[0,1,0,1]],"centers":[[36,74,41],[37,74,41]],"ignoreExtent":false,"flags":16448},"383":{"id":383,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[34,75,37]],"colors":[[0,1,0,1]],"centers":[[34,74,37],[34,75,37]],"ignoreExtent":false,"flags":16448},"384":{"id":384,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,38],[33,72,39]],"colors":[[0,1,0,1]],"centers":[[33,72,38],[33,72,39]],"ignoreExtent":false,"flags":16448},"385":{"id":385,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[33,71,40]],"colors":[[0,1,0,1]],"centers":[[33,71,39],[33,71,40]],"ignoreExtent":false,"flags":16448},"386":{"id":386,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[34,71,39]],"colors":[[0,1,0,1]],"centers":[[33,71,39],[34,71,39]],"ignoreExtent":false,"flags":16448},"387":{"id":387,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,73,43]],"colors":[[0,1,0,1]],"centers":[[37,73,42],[37,73,43]],"ignoreExtent":false,"flags":16448},"388":{"id":388,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,43],[37,68,43]],"colors":[[0,1,0,1]],"centers":[[37,67,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"389":{"id":389,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[33,68,45]],"colors":[[0,1,0,1]],"centers":[[33,67,45],[33,68,45]],"ignoreExtent":false,"flags":16448},"390":{"id":390,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,42],[34,70,43]],"colors":[[0,1,0,1]],"centers":[[34,70,42],[34,70,43]],"ignoreExtent":false,"flags":16448},"391":{"id":391,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[34,72,37]],"colors":[[0,1,0,1]],"centers":[[33,72,37],[34,72,37]],"ignoreExtent":false,"flags":16448},"392":{"id":392,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,68,43],[39,69,44]],"colors":[[0,0,1,1]],"centers":[[38,68,43],[39,69,44]],"ignoreExtent":false,"flags":16448},"393":{"id":393,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,71,45],[39,70,44]],"colors":[[0,0,1,1]],"centers":[[38,71,45],[39,70,44]],"ignoreExtent":false,"flags":16448},"394":{"id":394,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,44],[33,69,45]],"colors":[[0,0,1,1]],"centers":[[32,70,44],[33,69,45]],"ignoreExtent":false,"flags":16448},"395":{"id":395,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,72,39],[32,71,39]],"colors":[[0,0,1,1]],"centers":[[31,72,39],[32,71,39]],"ignoreExtent":false,"flags":16448},"396":{"id":396,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,41],[32,71,40]],"colors":[[0,0,1,1]],"centers":[[32,70,41],[32,71,40]],"ignoreExtent":false,"flags":16448},"397":{"id":397,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[34,68,45]],"colors":[[0,0,1,1]],"centers":[[34,67,45],[34,68,45]],"ignoreExtent":false,"flags":16448},"398":{"id":398,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,68,44],[36,68,43]],"colors":[[0,0,1,1]],"centers":[[35,68,44],[36,68,43]],"ignoreExtent":false,"flags":16448},"399":{"id":399,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,70,43],[32,70,44]],"colors":[[0,0,1,1]],"centers":[[31,70,43],[32,70,44]],"ignoreExtent":false,"flags":16448},"400":{"id":400,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,40],[31,72,39]],"colors":[[0,0,1,1]],"centers":[[31,71,40],[31,72,39]],"ignoreExtent":false,"flags":16448},"401":{"id":401,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,72,39],[31,73,38]],"colors":[[0,0,1,1]],"centers":[[31,72,39],[31,73,38]],"ignoreExtent":false,"flags":16448},"402":{"id":402,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,70,43],[32,70,42]],"colors":[[0,0,1,1]],"centers":[[31,70,43],[32,70,42]],"ignoreExtent":false,"flags":16448},"403":{"id":403,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,38],[35,75,39]],"colors":[[0,0,1,1]],"centers":[[35,75,38],[35,75,39]],"ignoreExtent":false,"flags":16448},"404":{"id":404,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[34,67,45]],"colors":[[0,0,1,1]],"centers":[[33,68,45],[34,67,45]],"ignoreExtent":false,"flags":16448},"405":{"id":405,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,73,38],[32,72,38]],"colors":[[0,0,1,1]],"centers":[[31,73,38],[32,72,38]],"ignoreExtent":false,"flags":16448},"406":{"id":406,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,74,37]],"colors":[[0,0,1,1]],"centers":[[33,73,37],[34,74,37]],"ignoreExtent":false,"flags":16448},"407":{"id":407,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,39],[33,71,39]],"colors":[[0,0,1,1]],"centers":[[32,71,39],[33,71,39]],"ignoreExtent":false,"flags":16448},"408":{"id":408,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,41],[34,70,41]],"colors":[[0,0,1,1]],"centers":[[33,71,41],[34,70,41]],"ignoreExtent":false,"flags":16448},"409":{"id":409,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,41],[33,70,42]],"colors":[[0,0,1,1]],"centers":[[32,70,41],[33,70,42]],"ignoreExtent":false,"flags":16448},"410":{"id":410,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,68,45],[35,68,44]],"colors":[[0,0,1,1]],"centers":[[34,68,45],[35,68,44]],"ignoreExtent":false,"flags":16448},"411":{"id":411,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,76,38],[35,75,38]],"colors":[[0,0,1,1]],"centers":[[34,76,38],[35,75,38]],"ignoreExtent":false,"flags":16448},"412":{"id":412,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,74,41]],"colors":[[0,0,1,1]],"centers":[[37,73,42],[37,74,41]],"ignoreExtent":false,"flags":16448},"413":{"id":413,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,36],[34,74,37]],"colors":[[0,0,1,1]],"centers":[[34,74,36],[34,74,37]],"ignoreExtent":false,"flags":16448},"414":{"id":414,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[37,72,44]],"colors":[[0,0,1,1]],"centers":[[37,71,45],[37,72,44]],"ignoreExtent":false,"flags":16448},"415":{"id":415,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,70,42],[33,70,41]],"colors":[[0,0,1,1]],"centers":[[32,70,42],[33,70,41]],"ignoreExtent":false,"flags":16448},"416":{"id":416,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,41],[34,71,40]],"colors":[[0,0,1,1]],"centers":[[34,70,41],[34,71,40]],"ignoreExtent":false,"flags":16448},"417":{"id":417,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,72,38],[32,73,37]],"colors":[[0,0,1,1]],"centers":[[32,72,38],[32,73,37]],"ignoreExtent":false,"flags":16448},"418":{"id":418,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,75,37],[34,76,38]],"colors":[[0,0,1,1]],"centers":[[34,75,37],[34,76,38]],"ignoreExtent":false,"flags":16448},"419":{"id":419,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,44],[38,72,44]],"colors":[[0,0,1,1]],"centers":[[37,73,44],[38,72,44]],"ignoreExtent":false,"flags":16448},"420":{"id":420,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,40],[35,71,39]],"colors":[[0,0,1,1]],"centers":[[34,71,40],[35,71,39]],"ignoreExtent":false,"flags":16448},"421":{"id":421,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,39],[36,75,40]],"colors":[[0,0,1,1]],"centers":[[35,75,39],[36,75,40]],"ignoreExtent":false,"flags":16448},"422":{"id":422,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,72,43],[38,73,42]],"colors":[[0,0,1,1]],"centers":[[38,72,43],[38,73,42]],"ignoreExtent":false,"flags":16448},"423":{"id":423,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,39],[31,72,39]],"colors":[[0,0,1,1]],"centers":[[31,71,39],[31,72,39]],"ignoreExtent":false,"flags":16448},"424":{"id":424,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,75,40]],"colors":[[0,0,1,1]],"centers":[[36,74,40],[36,75,40]],"ignoreExtent":false,"flags":16448},"425":{"id":425,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,40],[33,71,41]],"colors":[[0,0,1,1]],"centers":[[33,71,40],[33,71,41]],"ignoreExtent":false,"flags":16448},"426":{"id":426,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,41],[37,74,41]],"colors":[[0,0,1,1]],"centers":[[36,74,41],[37,74,41]],"ignoreExtent":false,"flags":16448},"427":{"id":427,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,71,40],[33,71,40]],"colors":[[0,0,1,1]],"centers":[[32,71,40],[33,71,40]],"ignoreExtent":false,"flags":16448},"428":{"id":428,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,72,43],[38,72,44]],"colors":[[0,0,1,1]],"centers":[[38,72,43],[38,72,44]],"ignoreExtent":false,"flags":16448},"429":{"id":429,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,73,37],[33,73,37]],"colors":[[0,0,1,1]],"centers":[[32,73,37],[33,73,37]],"ignoreExtent":false,"flags":16448},"430":{"id":430,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[33,69,45]],"colors":[[0,0,1,1]],"centers":[[33,68,45],[33,69,45]],"ignoreExtent":false,"flags":16448},"431":{"id":431,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,72,44],[37,73,44]],"colors":[[0,0,1,1]],"centers":[[37,72,44],[37,73,44]],"ignoreExtent":false,"flags":16448},"432":{"id":432,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[34,71,39]],"colors":[[0,0,1,1]],"centers":[[33,71,39],[34,71,39]],"ignoreExtent":false,"flags":16448},"433":{"id":433,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[38,73,42]],"colors":[[0,0,1,1]],"centers":[[37,73,42],[38,73,42]],"ignoreExtent":false,"flags":16448},"434":{"id":434,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,69,44],[39,70,44]],"colors":[[0,0,1,1]],"centers":[[39,69,44],[39,70,44]],"ignoreExtent":false,"flags":16448},"435":{"id":435,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,41],[33,70,42]],"colors":[[0,0,1,1]],"centers":[[33,70,41],[33,70,42]],"ignoreExtent":false,"flags":16448},"436":{"id":436,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[38,71,45]],"colors":[[0,0,1,1]],"centers":[[37,71,45],[38,71,45]],"ignoreExtent":false,"flags":16448},"437":{"id":437,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,39]],"colors":[[0,0,1,1]],"centers":[[34,71,39],[35,71,39]],"ignoreExtent":false,"flags":16448},"438":{"id":438,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,74,41]],"colors":[[0,0,1,1]],"centers":[[36,74,40],[36,74,41]],"ignoreExtent":false,"flags":16448},"439":{"id":439,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,75,36],[34,75,37]],"colors":[[0,0,1,1]],"centers":[[34,75,36],[34,75,37]],"ignoreExtent":false,"flags":16448},"440":{"id":440,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[31,71,39],[31,71,40]],"colors":[[0,0,1,1]],"centers":[[31,71,39],[31,71,40]],"ignoreExtent":false,"flags":16448},"441":{"id":441,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[38,68,43]],"colors":[[0,0,1,1]],"centers":[[37,68,43],[38,68,43]],"ignoreExtent":false,"flags":16448},"442":{"id":442,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,36],[34,75,36]],"colors":[[0,0,1,1]],"centers":[[34,74,36],[34,75,36]],"ignoreExtent":false,"flags":16448},"443":{"id":443,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,68,43],[37,68,43]],"colors":[[0,0,1,1]],"centers":[[36,68,43],[37,68,43]],"ignoreExtent":false,"flags":16448},"444":{"id":444,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[39,68,45],[39,69,44]],"colors":[[1,1,0,1]],"centers":[[39,68,45],[39,69,44]],"ignoreExtent":false,"flags":16448},"445":{"id":445,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,70,44],[34,70,43]],"colors":[[1,1,0,1]],"centers":[[33,70,44],[34,70,43]],"ignoreExtent":false,"flags":16448},"446":{"id":446,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,68,43],[37,69,42]],"colors":[[1,1,0,1]],"centers":[[37,68,43],[37,69,42]],"ignoreExtent":false,"flags":16448},"447":{"id":447,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,68,43],[39,69,44]],"colors":[[1,1,0,1]],"centers":[[38,68,43],[39,69,44]],"ignoreExtent":false,"flags":16448},"448":{"id":448,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,74,34],[35,75,34]],"colors":[[1,1,0,1]],"centers":[[35,74,34],[35,75,34]],"ignoreExtent":false,"flags":16448},"449":{"id":449,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[37,71,45]],"colors":[[1,1,0,1]],"centers":[[37,70,46],[37,71,45]],"ignoreExtent":false,"flags":16448},"450":{"id":450,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,69,46],[39,68,45]],"colors":[[1,1,0,1]],"centers":[[38,69,46],[39,68,45]],"ignoreExtent":false,"flags":16448},"451":{"id":451,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,68,43],[37,69,42]],"colors":[[1,1,0,1]],"centers":[[36,68,43],[37,69,42]],"ignoreExtent":false,"flags":16448},"452":{"id":452,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,72,37],[35,71,38]],"colors":[[1,1,0,1]],"centers":[[34,72,37],[35,71,38]],"ignoreExtent":false,"flags":16448},"453":{"id":453,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,40],[35,70,41]],"colors":[[1,1,0,1]],"centers":[[34,71,40],[35,70,41]],"ignoreExtent":false,"flags":16448},"454":{"id":454,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,75,35],[34,75,36]],"colors":[[1,1,0,1]],"centers":[[34,75,35],[34,75,36]],"ignoreExtent":false,"flags":16448},"455":{"id":455,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,73,37],[34,74,36]],"colors":[[1,1,0,1]],"centers":[[33,73,37],[34,74,36]],"ignoreExtent":false,"flags":16448},"456":{"id":456,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,74,41]],"colors":[[1,1,0,1]],"centers":[[37,73,42],[37,74,41]],"ignoreExtent":false,"flags":16448},"457":{"id":457,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,67,45],[35,66,44]],"colors":[[1,1,0,1]],"centers":[[34,67,45],[35,66,44]],"ignoreExtent":false,"flags":16448},"458":{"id":458,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,34],[36,74,35]],"colors":[[1,1,0,1]],"centers":[[35,75,34],[36,74,35]],"ignoreExtent":false,"flags":16448},"459":{"id":459,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,36],[37,73,37]],"colors":[[1,1,0,1]],"centers":[[36,74,36],[37,73,37]],"ignoreExtent":false,"flags":16448},"460":{"id":460,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,74,35],[36,74,36]],"colors":[[1,1,0,1]],"centers":[[35,74,35],[36,74,36]],"ignoreExtent":false,"flags":16448},"461":{"id":461,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[33,72,39]],"colors":[[1,1,0,1]],"centers":[[33,71,39],[33,72,39]],"ignoreExtent":false,"flags":16448},"462":{"id":462,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[34,68,45]],"colors":[[1,1,0,1]],"centers":[[33,67,45],[34,68,45]],"ignoreExtent":false,"flags":16448},"463":{"id":463,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,67,44],[35,68,43]],"colors":[[1,1,0,1]],"centers":[[35,67,44],[35,68,43]],"ignoreExtent":false,"flags":16448},"464":{"id":464,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,72,44],[37,73,43]],"colors":[[1,1,0,1]],"centers":[[37,72,44],[37,73,43]],"ignoreExtent":false,"flags":16448},"465":{"id":465,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,71,38],[35,72,37]],"colors":[[1,1,0,1]],"centers":[[35,71,38],[35,72,37]],"ignoreExtent":false,"flags":16448},"466":{"id":466,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,75,39]],"colors":[[1,1,0,1]],"centers":[[36,74,40],[36,75,39]],"ignoreExtent":false,"flags":16448},"467":{"id":467,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,70,46],[38,69,46]],"colors":[[1,1,0,1]],"centers":[[37,70,46],[38,69,46]],"ignoreExtent":false,"flags":16448},"468":{"id":468,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,42],[35,70,41]],"colors":[[1,1,0,1]],"centers":[[34,70,42],[35,70,41]],"ignoreExtent":false,"flags":16448},"469":{"id":469,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[35,75,37]],"colors":[[1,1,0,1]],"centers":[[34,74,37],[35,75,37]],"ignoreExtent":false,"flags":16448},"470":{"id":470,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,71,45],[37,72,44]],"colors":[[1,1,0,1]],"centers":[[37,71,45],[37,72,44]],"ignoreExtent":false,"flags":16448},"471":{"id":471,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,66,44],[35,67,43]],"colors":[[1,1,0,1]],"centers":[[35,66,44],[35,67,43]],"ignoreExtent":false,"flags":16448},"472":{"id":472,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,37],[34,75,36]],"colors":[[1,1,0,1]],"centers":[[34,74,37],[34,75,36]],"ignoreExtent":false,"flags":16448},"473":{"id":473,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,73,37],[33,73,37]],"colors":[[1,1,0,1]],"centers":[[32,73,37],[33,73,37]],"ignoreExtent":false,"flags":16448},"474":{"id":474,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,68,43],[36,67,43]],"colors":[[1,1,0,1]],"centers":[[35,68,43],[36,67,43]],"ignoreExtent":false,"flags":16448},"475":{"id":475,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,35],[36,74,36]],"colors":[[1,1,0,1]],"centers":[[36,74,35],[36,74,36]],"ignoreExtent":false,"flags":16448},"476":{"id":476,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,44],[37,68,43]],"colors":[[1,1,0,1]],"centers":[[37,67,44],[37,68,43]],"ignoreExtent":false,"flags":16448},"477":{"id":477,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,38],[34,72,37]],"colors":[[1,1,0,1]],"centers":[[33,72,38],[34,72,37]],"ignoreExtent":false,"flags":16448},"478":{"id":478,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,69,45],[33,70,44]],"colors":[[1,1,0,1]],"centers":[[33,69,45],[33,70,44]],"ignoreExtent":false,"flags":16448},"479":{"id":479,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,67,44],[38,67,43]],"colors":[[1,1,0,1]],"centers":[[37,67,44],[38,67,43]],"ignoreExtent":false,"flags":16448},"480":{"id":480,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,36],[37,73,36]],"colors":[[1,1,0,1]],"centers":[[36,74,36],[37,73,36]],"ignoreExtent":false,"flags":16448},"481":{"id":481,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[32,73,37],[33,72,37]],"colors":[[1,1,0,1]],"centers":[[32,73,37],[33,72,37]],"ignoreExtent":false,"flags":16448},"482":{"id":482,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,40],[35,71,39]],"colors":[[1,1,0,1]],"centers":[[34,71,40],[35,71,39]],"ignoreExtent":false,"flags":16448},"483":{"id":483,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,34],[34,75,35]],"colors":[[1,1,0,1]],"centers":[[34,74,34],[34,75,35]],"ignoreExtent":false,"flags":16448},"484":{"id":484,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,72,37],[35,72,37]],"colors":[[1,1,0,1]],"centers":[[34,72,37],[35,72,37]],"ignoreExtent":false,"flags":16448},"485":{"id":485,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[34,68,45]],"colors":[[1,1,0,1]],"centers":[[33,68,45],[34,68,45]],"ignoreExtent":false,"flags":16448},"486":{"id":486,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,75,38],[36,75,39]],"colors":[[1,1,0,1]],"centers":[[36,75,38],[36,75,39]],"ignoreExtent":false,"flags":16448},"487":{"id":487,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,67,43],[35,67,44]],"colors":[[1,1,0,1]],"centers":[[35,67,43],[35,67,44]],"ignoreExtent":false,"flags":16448},"488":{"id":488,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,42],[37,73,43]],"colors":[[1,1,0,1]],"centers":[[37,73,42],[37,73,43]],"ignoreExtent":false,"flags":16448},"489":{"id":489,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,38],[36,75,38]],"colors":[[1,1,0,1]],"centers":[[35,75,38],[36,75,38]],"ignoreExtent":false,"flags":16448},"490":{"id":490,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[35,75,37],[35,75,38]],"colors":[[1,1,0,1]],"centers":[[35,75,37],[35,75,38]],"ignoreExtent":false,"flags":16448},"491":{"id":491,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,34],[35,74,34]],"colors":[[1,1,0,1]],"centers":[[34,74,34],[35,74,34]],"ignoreExtent":false,"flags":16448},"492":{"id":492,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,67,45],[34,67,45]],"colors":[[1,1,0,1]],"centers":[[33,67,45],[34,67,45]],"ignoreExtent":false,"flags":16448},"493":{"id":493,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[38,67,43],[38,68,43]],"colors":[[1,1,0,1]],"centers":[[38,67,43],[38,68,43]],"ignoreExtent":false,"flags":16448},"494":{"id":494,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,41],[37,74,41]],"colors":[[1,1,0,1]],"centers":[[36,74,41],[37,74,41]],"ignoreExtent":false,"flags":16448},"495":{"id":495,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,35],[34,74,36]],"colors":[[1,1,0,1]],"centers":[[34,74,35],[34,74,36]],"ignoreExtent":false,"flags":16448},"496":{"id":496,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,70,42],[34,70,43]],"colors":[[1,1,0,1]],"centers":[[34,70,42],[34,70,43]],"ignoreExtent":false,"flags":16448},"497":{"id":497,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,74,40],[36,74,41]],"colors":[[1,1,0,1]],"centers":[[36,74,40],[36,74,41]],"ignoreExtent":false,"flags":16448},"498":{"id":498,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[36,67,43],[36,68,43]],"colors":[[1,1,0,1]],"centers":[[36,67,43],[36,68,43]],"ignoreExtent":false,"flags":16448},"499":{"id":499,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,37],[34,72,37]],"colors":[[1,1,0,1]],"centers":[[33,72,37],[34,72,37]],"ignoreExtent":false,"flags":16448},"500":{"id":500,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[37,73,36],[37,73,37]],"colors":[[1,1,0,1]],"centers":[[37,73,36],[37,73,37]],"ignoreExtent":false,"flags":16448},"501":{"id":501,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,71,39],[34,71,39]],"colors":[[1,1,0,1]],"centers":[[33,71,39],[34,71,39]],"ignoreExtent":false,"flags":16448},"502":{"id":502,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,68,45],[33,69,45]],"colors":[[1,1,0,1]],"centers":[[33,68,45],[33,69,45]],"ignoreExtent":false,"flags":16448},"503":{"id":503,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,71,39],[35,71,39]],"colors":[[1,1,0,1]],"centers":[[34,71,39],[35,71,39]],"ignoreExtent":false,"flags":16448},"504":{"id":504,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[34,74,35],[35,74,35]],"colors":[[1,1,0,1]],"centers":[[34,74,35],[35,74,35]],"ignoreExtent":false,"flags":16448},"505":{"id":505,"type":"linestrip","material":{"lit":false,"lwd":5},"vertices":[[33,72,38],[33,72,39]],"colors":[[1,1,0,1]],"centers":[[33,72,38],[33,72,39]],"ignoreExtent":false,"flags":16448},"173":{"id":173,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"172":{"id":172,"type":"background","material":{"fog":true},"colors":[[0.2980392,0.2980392,0.2980392,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"174":{"id":174,"type":"background","material":{"lit":false,"back":"lines"},"colors":[[1,1,1,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"169":{"id":169,"type":"subscene","par3d":{"antialias":0,"FOV":30,"ignoreExtent":false,"listeners":169,"mouseMode":{"left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,80.23582],"modelMatrix":[[0.3630731,-0.5154495,0.7762021,-11.68474],[0.7040736,0.6974096,0.1337916,-81.13914],[-0.6102936,0.4979273,0.616125,-115.2523],[0,0,0,1]],"projMatrix":[[2.665751,0,0,0],[0,3.732051,0,0],[0,0,-3.863704,-289.2409],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[0.3630731,-0.5154495,0.7762021,0],[0.7040736,0.6974096,0.1337916,0],[-0.6102936,0.4979273,0.616125,0],[0,0,0,1]],"scale":[1,1,1],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[30,50,57,79,27,56],"windowRect":[100,128,772,608],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"/home/dyslexicon/R/x86_64-pc-linux-gnu-library/3.2/rgl/fonts/FreeSans.ttf","maxClipPlanes":8,"glVersion":3},"embeddings":{"viewport":"replace","projection":"replace","model":"replace"},"objects":[174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,173],"subscenes":[],"flags":21056}},"snapshot":"data:image/png;base64,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","width":673,"height":481,"sphereVerts":{"reuse":"unnamed_chunk_3div"},"context":{"shiny":false,"rmarkdown":"github_document"},"crosstalk":{"key":[],"group":[],"id":[],"options":[]}});
unnamed_chunk_6rgl.prefix = "unnamed_chunk_6";
</script>
<p id="unnamed_chunk_6debug">
You must enable Javascript to view this page properly.
</p>
<script>unnamed_chunk_6rgl.start();</script>
