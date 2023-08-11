import numpy
import time

from f_qrmod import qrmod

n = 5
a = numpy.random.rand(n,n,)
b = numpy.random.rand(n)
print(a)

print("Expected (b):")
print(b)

#qrmod.normal_solve( 3, 3, a, b, x )
print("QR Solve")
x = numpy.zeros((n), dtype=numpy.float64)
qrmod.qr_solve(a, b, x)
print(x)

print("Expected (Ax) (b):")
print(numpy.matmul(a, x))
print()

print("Normal Solve")
x = numpy.zeros((n), dtype=numpy.float64)
qrmod.normal_solve( a, b, x )
print(x)
print("Expected (Ax) (b):")
print(numpy.matmul(a, x))
print()

print("SVD Solve")
x = numpy.zeros((n), dtype=numpy.float64)
qrmod.svd_solve( a, b, x )
print(x)
print("Expected (Ax) (b):")
print(numpy.matmul(a, x))
print()






a = numpy.asarray([[1,	-1.5,	-1, 1.5,	2.25	,1	,-2.25,	-1.5,	2.25],
[1,	-1.5,	-0.6,	0.9,	2.25,	0.36,	-1.35,	-0.54,	0.81],
[1,	-1.5,	-0.2,	0.3,	2.25,	0.04,	-0.45,	-0.06,	0.09],
[1,	-1.1,	-1,	1.1,	1.21,	1,	-1.21,	-1.1,	1.21],
[1,	-1.1,	-0.6,	0.66,	1.21,	0.36,	-0.726,	-0.396,	0.4356],
[1,	-1.1,	-0.2,	0.22,	1.21,	0.04,	-0.242,	-0.044,	0.0484],
[1,	-0.7,	-1,	0.7,	0.49,	1,	-0.49,	-0.7,	0.49],
[1,	-0.7,	-0.6,	0.42,	0.49,	0.36,	-0.294, -0.252,	0.1764],
[1,	-0.7,	-0.2,	0.14,	0.49,	0.04,	-0.098,	-0.028,	0.0196]], dtype=numpy.float64)
b = numpy.asarray([-0.46,-0.17,-0.02,-0.35,-0.07,0.09,-0.1,0.18,0.34], dtype=numpy.float64)

x = numpy.zeros((9), dtype=numpy.float64)
qrmod.normal_solve(a, b, x)
print(x)
x = numpy.zeros((9), dtype=numpy.float64)
qrmod.qr_solve(a, b, x)
print(x)
x = numpy.zeros((9), dtype=numpy.float64)
qrmod.svd_solve(a, b, x)
print(x)


"""
1.11929688  1.3421875  -0.08046875 -0.421875    0.3984375  -0.52539062
 -0.234375   -0.3515625  -0.1953125
"""


print()

r = 1000
n = 100
a = numpy.random.rand(n,n,)
b = numpy.random.rand(n)
x = numpy.zeros((n), dtype=numpy.float64)

ta = time.time()
for m in range(100):
  qrmod.normal_solve(a, b, x)
bnew = numpy.matmul(a, x)
print("NORMAL", sum(abs(b[:] - bnew[:])), time.time() - ta)

ta = time.time()
for m in range(100):
  qrmod.qr_solve(a, b, x)
bnew = numpy.matmul(a, x)
print("QR    ",sum(abs(b[:] - bnew[:])), time.time() - ta)

ta = time.time()
for m in range(100):
  qrmod.svd_solve(a, b, x)
bnew = numpy.matmul(a, x)
print("SVD   ",sum(abs(b[:] - bnew[:])), time.time() - ta)














