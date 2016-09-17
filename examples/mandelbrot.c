#include <stdlib.h>
#include <stdio.h>

long println(long val) {
	printf("%ld\n", val);
	return 0;
}
long print(long val) {
	printf("%ld", val);
	return 0;
}
long read(void) {
	long res;
	scanf("%ld", &res);
	return res;
}
long get_iter(long px, long py) {
 long _0temp31 = 0, _0temp40 = 0, _0temp17 = 0, _0temp42 = 0, _0temp28 = 0, _0temp15 = 0, _0temp25 = 0, _0temp14 = 0, _0temp5 = 0, y = 0, max_iter = 0, _0temp33 = 0, _0temp6 = 0, _0temp18 = 0, x = 0, yi = 0, _0temp49 = 0, _0temp0 = 0, iter = 0, _0temp34 = 0, _0temp1 = 0, _0temp39 = 0, _0temp29 = 0, xi = 0, _0temp44 = 0;
label0:
	_0temp1 = (5l * px);
	_0temp0 = (_0temp1 - 250l);
	xi = (_0temp0);
	_0temp6 = (5l * py);
	_0temp5 = (_0temp6 - 100l);
	yi = (_0temp5);
	x = (0l);
	y = (0l);
	iter = (0l);
	max_iter = (1000l);
	goto label6;
label6:
	_0temp17 = (x * x);
	_0temp18 = (y * y);
	_0temp15 = (_0temp17 + _0temp18);
	_0temp14 = (_0temp15 < 40000l);
	if (_0temp14) { goto label7; } else { goto label8; }
label7:
	_0temp25 = (iter >= max_iter);
	if (_0temp25) { goto label8; } else { goto label13; }
label13:
	_0temp33 = (x * x);
	_0temp34 = (y * y);
	_0temp31 = (_0temp33 - _0temp34);
	_0temp29 = (_0temp31 / 100l);
	_0temp28 = (_0temp29 + xi);
	_0temp44 = (2l * x);
	_0temp42 = (_0temp44 * y);
	_0temp40 = (_0temp42 / 100l);
	_0temp39 = (_0temp40 + yi);
	y = (_0temp39);
	x = (_0temp28);
	_0temp49 = (iter + 1l);
	iter = (_0temp49);
	goto label6;
label8:
	return iter;
 }
long mod(long x, long m) {
 long _0temp53 = 0, _0temp59 = 0, _0temp56 = 0, _0temp62 = 0;
label24:
	_0temp53 = (x < 0l);
	if (_0temp53) { goto label25; } else { goto label29; }
label25:
	_0temp56 = (x + m);
	x = (_0temp56);
	goto label24;
label29:
	_0temp59 = (x >= m);
	if (_0temp59) { goto label30; } else { goto label31; }
label30:
	_0temp62 = (x - m);
	x = (_0temp62);
	goto label29;
label31:
	return x;
 }
int main() {
 long _0temp79 = 0, _0temp71 = 0, _0temp74 = 0, _0temp83 = 0, _0temp75 = 0, _0temp68 = 0, px = 0, py = 0;
label36:
	px = (0l);
	py = (0l);
	goto label38;
label38:
	_0temp68 = (py < 40l);
	if (_0temp68) { goto label41; } else { goto label40; }
label41:
	_0temp71 = (px < 70l);
	if (_0temp71) { goto label42; } else { goto label43; }
label42:
	_0temp75 = get_iter(px, py);
	_0temp74 = mod(_0temp75, 10l);
	print(_0temp74);
	_0temp79 = (px + 1l);
	px = (_0temp79);
	goto label41;
label43:
	println(0l);
	_0temp83 = (py + 1l);
	py = (_0temp83);
	px = (0l);
	goto label38;
label40:
	return 0l;
 }
