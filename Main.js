
//Programas 2 - Testando While
/*
var x = 0, y = 10;
x + y;
if (x == 1) {
	var z = true;
}
else {
	var x = 10;
}
while (x < 15) {
	x = x+1;
}
*/

//Programa 3 - Testando for
/*
var x = 1, y = 10;
x + y;
if (x == 1) {
	p = 100;
	var z = true;
}
else {
	var x = 10;
}
for (; x < 15;) {
	x = x+1
}
*/

//Programa 4 - Testando while e for
/*
var x = 0, y = 10;
x + y;
if (x == 1) {
	var z = true;
}
else {
	var x = 10;
}
while (x < 15) {
	x = x+1;
	for (; x < 20;) {
		x = x+1
	}
}
*/

//Programa 5 - Testando break
/*
x = 20;
for (var i = 0;  ; i = i + 1){
	break;
	x = 30;
}
if (x == 20) {
	break;
	x = 30;
}
*/

/*
function dez(a,b,c){
	return a + b + c;
}

function fatorial (n){
	var ola = 10;
	if (n > 0){
		return n*fatorial (n - 1)
	} 
	else {
        return 1
	}
}

dez(1,1,1);

var x = fatorial (2);
var y = fatorial (3);
var z = fatorial (4);
var h = fatorial (5);

*/


/*
z = [12,3,4];
A = [2,4,2,3];

var ola = z.head();
var ola2 = z.tail();
var ola3 = z.concat(A);

o = z;
*/


// Length function

function len(A){
	if (A == []){
		return 0;
	}
	else {
		return 1 + len(A.tail())
	}
}

/*
A = [1,2,3,4,5,6,7,8]
B = []
C = [2,3,4]


a = len(A);
b = len(B);
c = len(C);

if (A == C) {
	ola = "Cade marcio";
}
*/
