#include <R.h>
#include <R_ext/Lapack.h>
#include <stdio.h>




void qrc(int *MX, int *NX, double *X, int *LDX, double *tau, int outlwork)
  {
    int info;
    double work[1];
    int lwork;


    lwork = -1;

    F77_CALL(dgeqrf)(MX, NX, X, LDX, tau, work, &lwork, &info);
      /*result = X*X;*/

    if (info != 0)
      error("QR decomposition failed");

    outlwork = (int) work[0];




    double work2[outlwork];

    F77_CALL(dgeqrf)(MX, NX, X, LDX, tau, work2, &outlwork, &info);


}





