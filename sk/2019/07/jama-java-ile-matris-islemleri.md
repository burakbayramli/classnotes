# JAMA - Java ile Matris İşlemleri

Python ile prototip hatta servis tarafı sayısal işlem kodları rahatça
yazılıyor; fakat bazen lineer cebir yapan kodları Java'ya çevirmek
gerekebilir, mesela Android üzerinde işlemesi gereken kodlar. Pür Java
ile yazılmış kullanışlı liner cebir kütüphanelerinden biri JAMA:

http://math.nist.gov/javanumerics/jama/#Package

Matrix Class Doc

İhtiyaç olan en önemli özellikler temel işlemler, toplama, çıkartma,
ve matris arası çarpım, matris tersi (matrix inversion) ve devriği
(transpose) Ek iyi olabilecek özellikler en az kareler (least squares)
çözümü, SVD, LU, Cholesky ayrıştırması, vs. Bu özelliklerin hepsi
Jama'da var.

Üstteki ilk bağlantıdan jar indirilir, altta kısa bir test,

[Link](jama1.txt)

Sonuclar

```
A
0.0 0.0 0.0 
10.0 10.0 10.0 
0.0 0.0 0.0 

A get
10.0
B
1.0 2.0 3.0 
4.0 5.0 6.0 
7.0 8.0 10.0 

Carpim
0.0 0.0 0.0 
120.0 150.0 190.0 
0.0 0.0 0.0 

Toplam
1.0 2.0 3.0 
14.0 15.0 16.0 
7.0 8.0 10.0 
```

Bazı püf noktaları:

Android ortamı her ne kadar tam gömülü (embedded) bir ortam sayılmasa
da, servis tarafı kodlanıyormuş gibi davranmaktan kaçınmak iyi olur,
mesela telefon ivmeölçerinden gelen verileri hızlı bir şekilde işlemek
gerekiyor, her işlem için bir matris çarpımı lazım, bu durumda her
yeni veri iletimi için yeni matrisler yaratıp onları çarpmaya gerek
yok, her seferinde hafızaya obje ekleme, onun çöp toplayıcısı
tarafından silinmesi işleri ağırlaştırır. En iyisi sadece iki Matrix
objesi yaratıp ama değerlerini her seferinde değiştirip çarpımı bu
aynı objeler üzerinde yapmak.

Ek kodlar, mesela Jama kodlarında çapraz çarpım verilmemiş, bu benzeri
ekler,

```
import Jama.Matrix;
import java.net.*;
import java.util.*;
import java.io.*;
import java.text.*;

public class Util {

    // Helper class hidden here to provide additional matrix methods
    // taken from https://github.com/thorstenwagner/ij-ellipsesplit
    public static class Matrix2  {
        private double[][] A;
        private int m, n;
        public Matrix2(int m, int n) {
            this.m = m;
            this.n = n;
            A = new double[m][n];
        }
        public Matrix2(double[][] A) {
            m = A.length;
            n = A[0].length;
            for (int i = 0; i < m; i++) {
                if (A[i].length != n) {
                    throw new IllegalArgumentException("All rows must have the same length.");
                }
            }
            this.A = A;
        }

        public Matrix2 crossProduct(Matrix2 B) {
            if (m != B.m || n != B.n) {
                throw new IllegalArgumentException("Matrix dimensions must agree");
            }
            Matrix2 X = new Matrix2(m, n);
            if (m == 1 && n == 3) {
                X.A[0][0] = A[0][1] * B.A[0][2] - A[0][2] * B.A[0][1];
                X.A[0][1] = A[0][2] * B.A[0][0] - A[0][0] * B.A[0][2];
                X.A[0][2] = A[0][0] * B.A[0][1] - A[0][1] * B.A[0][0];
            } else if (m == 3 && n == 1) {
                X.A[0][0] = A[1][0] * B.A[2][0] - A[2][0] * B.A[1][0];
                X.A[1][0] = A[2][0] * B.A[0][0] - A[0][0] * B.A[2][0];
                X.A[2][0] = A[0][0] * B.A[1][0] - A[1][0] * B.A[0][0];
            } else {
                throw new IllegalArgumentException(
                                                   "Matrix must be a 3-element row or column vector");
            }
            return X;
        }

        public double[][] getArray() {
            return A;
        }
    }

    public static String toString(Matrix m) {
        String s = "";
        for (int i=0;i<m.getRowDimension(); i++){
            for (int j=0;j<m.getColumnDimension(); j++){
                s += "" + m.get(i,j) + " ";
            }
            s += "\n";
        }
        return s;
    }

    // project u onto plane with normal n
    public static Matrix proj (double[] u, double[] n) {
        Matrix um = new Matrix(u, 3);
        Matrix un = new Matrix(n, 3);
        Matrix tmp1 = um.transpose().times(un);
        Matrix tmp2 = un.transpose().times(un);
        Matrix tmp3 = un.times(tmp1.get(0,0) / tmp2.get(0,0));
        Matrix res = um.minus(tmp3);
        return res;
    }

    public static Matrix cross(Matrix a, Matrix b) {
        Matrix2 aa = new Matrix2(a.getArray());
        Matrix2 bb = new Matrix2(b.getArray());
        Matrix2 res = aa.crossProduct(bb);
        return new Matrix(res.getArray());
     }
}
```

