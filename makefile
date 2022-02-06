
FC = /home/mf/dp/marp/gco/apps/mkpack/support/wrapper/mimpifc-18.0.5.274 -convert big_endian -assume byterecl -align array64byte,all -traceback -fpic -qopenmp -qopenmp-threadprivate compat -fp-model source -qopt-report=5 -qopt-report-phase=vec -ftz -diag-disable=remark,cpu-dispatch -r8 -g -O2 -march=core-avx2 -finline-functions -finline-limit=500 -Winline -qopt-prefetch=4 -fast-transcendentals -fimf-use-svml -no-fma

main.x: main.F90
	$(FC) -o main.x main.F90
