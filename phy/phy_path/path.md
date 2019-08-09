#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Vektör Formu

Benzer bir problemi vektörel formda çözelim. Bir uçağın rüzgarlı bir
bölgede gitmesi gerekiyor. Rüzgarın büyüklüğünü ve yönünü bir vektör
alanı olarak temsil edebiliriz, rüzgar pozisyonun bir fonksiyonudur,
$w = \vec{w}, r = \vec{r}, u = \vec{u}$ olmak üzere, rüzgar $w =
w(r)$, ki $r = \left[\begin{array}{ccc} r_x, r_y, r_z
\end{array}\right]^T$ 
üç boyutlu pozisyonu temsil ediyor [1, sf. 96]. Ucagin hizi $V$
sabit. 

Problem uçağın yönünü her $t$ anında optimal şekilde ayarlayabilmek
öyle ki A noktasından B noktasına, verili rüzgar alanı içinden en kısa
şekilde gidebilmek. 

Yere kıyasla uçağın toplam hızı 

$$
\dot{r} = V \hat{u} + w
$$

ki $\hat{u}$ ucagin yonunu gosteren birim vektor, $\hat{u} \cdot
\hat{u} = 1$. 

\includegraphics[width=15em]{phy_path_03.png}

Üstteki resim toplamsal hızı temsil eden vektörsel toplamı temsili
olarak gösteriyor. 

Hamiltonian 

$$
\mathcal{H} = \lambda \cdot (V \hat{u}  + w) + 
\mu (1 - \hat{u}\cdot\hat{u}) + 1
$$

Euler-Lagrange denklemleri 

$$
\dot{\lambda} = - \frac{\partial H}{\partial r} = - \nabla (\lambda \cdot w)
$$

$$
0 = \frac{\partial H}{\partial \hat{u}} = V \lambda - 2\mu\hat{u}
$$















