
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
var x = 0, y = 10;
x + y;
if (x == 1) {
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


function dez(a,b,c){
	return a + b + c;
}

function fatorial (n){
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





/*
//Programa 10 - Testando Quicksort recursivamente
function quicksort (A, esquerda, direita) {
	if (esquerda<direita) {
		particao = particiona(A, esquerda, direita);
		quicksort(A, esquerda, particao-1);
		quicksort(A, particao+1, direita);
	}
	return A
}
    
	
function particiona (A, esquerda, direita) {
		
	i=esquerda;
	j=direita;
	pivo=A[esquerda];
	troca = 0;
	troca2 = 0;
	while (i<j) {
		
		while (A[i]<=pivo && i+1<A.length) {
			i++;		
		}
		
		while (A[j]>pivo && j-1>0) {
			
			j--;
		}
		
		if (i<j) {
			troca = A[i];
			troca2 = A[j];
			A[i]=troca2;
			A[j]=troca;
		}			
	}
	troca = A[esquerda];
	troca2 = A[j];
	A[esquerda]=troca2;
	A[j]=troca;
	return j;
}

A = [6, 34, 78, 98, 7, 56, 4, 90, 27, 2];
B = quicksort(A, 0, 9);
*/

/*
z = [12,3,4];
A = [2,4,2,3];

var ola = z.head();
var ola2 = z.tail();
var ola3 = z.concat(A);

o = z;
*/