use typst_math_rust::parse_document;

/// Usefull to test the library in pure rust
fn main() {
    let parsed = parse_document("$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $$alpha^alpha^(alpha)_beta^alpha$
    $(alpha)^alpha alpha^(-alpha=) -->_(alpha+2)
    
    Im(e)$
    $ beta$
    
    
    
    == Basic tests and examples
    $
      forall x in RR, exists y in RR, x=2 y \\
    $
    
    $
      forall exists in in.not in.small subset subset.not subset.eq subset.eq.not union union.big sect sect.big complement \\
      RR_+ RR_- RR^* RR_+^* RR^*_+ RR_-^* RR^*_- \\
    $
    = Superscript and subscript tests
    $
      x^a x^b x^c x^d x^e x^f x^g x^h x^i x^j x^k x^l x^m x^n x^o x^p x^q x^r x^s x^t x^u x^v x^w x^x x^y x^z x^0 x^1 x^2 x^3 x^4 x^5 x^6 x^7 x^8 x^9 \\
      x^(a) x^(b) x^(c) x^(d) x^(e) x^(f) x^(g) x^(h) x^(i) x^(j) x^(k) x^(l) x^(m) x^(n) x^(o) x^(p) x^(q) x^(r) x^(s) x^(t) x^(u) x^(v) x^(w) x^(x) x^(y) x^(z) x^(0) x^(1) x^(2) x^(3) x^(4) x^(5) x^(6) x^(7) x^(8) x^(9) \\
      x^(-a) x^(-b) x^(-c) x^(-d) x^(-e) x^(-f) x^(-g) x^(-h) x^(-i) x^(-j) x^(-k) x^(-l) x^(-m) x^(-n) x^(-o) x^(-p) x^(-q) x^(-r) x^(-s) x^(-t) x^(-u) x^(-v) x^(-w) x^(-x) x^(-y) x^(-z) x^(-0) x^(-1) x^(-2) x^(-3) x^(-4) x^(-5) x^(-6) x^(-7) x^(-8) x^(-9) x^(-10) \\
      x_a x_b x_c x_d x_e x_f x_g x_h x_i x_j x_k x_l x_m x_n x_o x_p x_q x_r x_s x_t x_u x_v x_w x_x x_y x_z x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 \\
      x_(a) x_(b) x_(c) x_(d) x_(e) x_(f) x_(g) x_(h) x_(i) x_(j) x_(k) x_(l) x_(m) x_(n) x_(o) x_(p) x_(q) x_(r) x_(s) x_(t) x_(u) x_(v) x_(w) x_(x) x_(y) x_(z) x_(0) x_(1) x_(2) x_(3) x_(4) x_(5) x_(6) x_(7) x_(8) x_(9) \\
      x_(-a) x_(-b) x_(-c) x_(-d) x_(-e) x_(-f) x_(-g) x_(-h) x_(-i) x_(-j) x_(-k) x_(-l) x_(-m) x_(-n) x_(-o) x_(-p) x_(-q) x_(-r) x_(-s) x_(-t) x_(-u) x_(-v) x_(-w) x_(-x) x_(-y) x_(-z) x_(-0) x_(-1) x_(-2) x_(-3) x_(-4) x_(-5) x_(-6) x_(-7) x_(-8) x_(-9) x_(-10) \\
      x^alpha x^Alpha x^beta x^Beta x^gamma x^Gamma x^delta x^Delta x^epsilon x^Epsilon x^zeta x^Zeta x^eta x^Eta x^theta x^Theta x^iota x^Iota x^kappa x^Kappa x^lambda x^Lambda x^mu x^Mu x^nu x^Nu x^xi x^Xi x^omicron x^Omicron x^pi x^Pi x^rho x^Rho x^sigma x^Sigma x^tau x^Tau x^upsilon x^Upsilon x^phi x^Phi x^chi x^Chi x^psi x^Psi x^omega x^Omega \\
      x^(alpha) x^(Alpha) x^(beta) x^(Beta) x^(gamma) x^(Gamma) x^(delta) x^(Delta) x^(epsilon) x^(Epsilon) x^(zeta) x^(Zeta) x^(eta) x^(Eta) x^(theta) x^(Theta) x^(iota) x^(Iota) x^(kappa) x^(Kappa) x^(lambda) x^(Lambda) x^(mu) x^(Mu) x^(nu) x^(Nu) x^(xi) x^(Xi) x^(omicron) x^(Omicron) x^(pi) x^(Pi) x^(rho) x^(Rho) x^(sigma) x^(Sigma) x^(tau) x^(Tau) x^(upsilon) x^(Upsilon) x^(phi) x^(Phi) x^(chi) x^(Chi) x^(psi) x^(Psi) x^(omega) x^(Omega) \\
      x^(-alpha) x^(-Alpha) x^(-beta) x^(-Beta) x^(-gamma) x^(-Gamma) x^(-delta) x^(-Delta) x^(-epsilon) x^(-Epsilon) x^(-zeta) x^(-Zeta) x^(-eta) x^(-Eta) x^(-theta) x^(-Theta) x^(-iota) x^(-Iota) x^(-kappa) x^(-Kappa) x^(-lambda) x^(-Lambda) x^(-mu) x^(-Mu) x^(-nu) x^(-Nu) x^(-xi) x^(-Xi) x^(-omicron) x^(-Omicron) x^(-pi) x^(-Pi) x^(-rho) x^(-Rho) x^(-sigma) x^(-Sigma) x^(-tau) x^(-Tau) x^(-upsilon) x^(-Upsilon) x^(-phi) x^(-Phi) x^(-chi) x^(-Chi) x^(-psi) x^(-Psi) x^(-omega) x^(-Omega) \\
      x_alpha x_Alpha x_beta x_Beta x_gamma x_Gamma x_delta x_Delta x_epsilon x_Epsilon x_zeta x_Zeta x_eta x_Eta x_theta x_Theta x_iota x_Iota x_kappa x_Kappa x_lambda x_Lambda x_mu x_Mu x_nu x_Nu x_xi x_Xi x_omicron x_Omicron x_pi x_Pi x_rho x_Rho x_sigma x_Sigma x_tau x_Tau x_upsilon x_Upsilon x_phi x_Phi x_chi x_Chi x_psi x_Psi x_omega x_Omega \\
      x_(alpha) x_(Alpha) x_(beta) x_(Beta) x_(gamma) x_(Gamma) x_(delta) x_(Delta) x_(epsilon) x_(Epsilon) x_(zeta) x_(Zeta) x_(eta) x_(Eta) x_(theta) x_(Theta) x_(iota) x_(Iota) x_(kappa) x_(Kappa) x_(lambda) x_(Lambda) x_(mu) x_(Mu) x_(nu) x_(Nu) x_(xi) x_(Xi) x_(omicron) x_(Omicron) x_(pi) x_(Pi) x_(rho) x_(Rho) x_(sigma) x_(Sigma) x_(tau) x_(Tau) x_(upsilon) x_(Upsilon) x_(phi) x_(Phi) x_(chi) x_(Chi) x_(psi) x_(Psi) x_(omega) x_(Omega) \\
      x_(-alpha) x_(-Alpha) x_(-beta) x_(-Beta) x_(-gamma) x_(-Gamma) x_(-delta) x_(-Delta) x_(-epsilon) x_(-Epsilon) x_(-zeta) x_(-Zeta) x_(-eta) x_(-Eta) x_(-theta) x_(-Theta) x_(-iota) x_(-Iota) x_(-kappa) x_(-Kappa) x_(-lambda) x_(-Lambda) x_(-mu) x_(-Mu) x_(-nu) x_(-Nu) x_(-xi) x_(-Xi) x_(-omicron) x_(-Omicron) x_(-pi) x_(-Pi) x_(-rho) x_(-Rho) x_(-sigma) x_(-Sigma) x_(-tau) x_(-Tau) x_(-upsilon) x_(-Upsilon) x_(-phi) x_(-Phi) x_(-chi) x_(-Chi) x_(-psi) x_(-Psi) x_(-omega) x_(-Omega) \\
    $
    = Big letters
    $
      sum_(k=1)^(n)
      product_(k=1)^(n)
      integral_0^1 x^2 dif x \\
    $
    = Sets
    $
      AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO PP QQ RR SS TT UU VV WW XX YY ZZ emptyset [| |] [ ] \\
      AA_AA BB_BB CC_CC DD_DD EE_EE FF_FF GG_GG HH_HH II_II JJ_JJ KK_KK LL_LL MM_MM NN_NN OO_OO PP_PP QQ_QQ RR_RR SS_SS TT_TT UU_UU VV_VV WW_WW XX_XX YY_YY ZZ_ZZ emptyset_emptyset \\
      AA^AA BB^BB CC^CC DD^DD EE^EE FF^FF GG^GG HH^HH II^II JJ^JJ KK^KK LL^LL MM^MM NN^NN OO^OO PP^PP QQ^QQ RR^RR SS^SS TT^TT UU^UU VV^VV WW^WW XX^XX YY^YY ZZ^ZZ emptyset^emptyset \\
      
      |wj-zwj-zwnj-zws-space-space.nobreak-space.en-space.quad-space.third-space.quarter-space.sixth-space.med-space.fig-space.punct-space.thin-space.hair-hyph.soft| \\
      = := ::= =: != => || |=> ==> <== <=> <==> <= >= < > << >> <<< >>> -> --> ->> ~> ~~> >-> |-> <- <-- <<- <~ <~~ <-< <-> <--> \\
    
      + - * \\
    
      cal(A) cal(B) cal(C) cal(D) cal(E) cal(F) cal(G) cal(H) cal(I) cal(J) cal(K) cal(L) cal(M) cal(N) cal(O) cal(P) cal(Q) cal(R) cal(S) cal(T) cal(U) cal(V) cal(W) cal(X) cal(Y) cal(Z) cal(a) cal(b) cal(c) cal(d) cal(e) cal(f) cal(g) cal(h) cal(i) cal(j) cal(k) cal(l) cal(m) cal(n) cal(o) cal(p) cal(q) cal(r) cal(s) cal(t) cal(u) cal(v) cal(w) cal(x) cal(y) cal(z) \\
      frak(A) frak(B) frak(C) frak(D) frak(E) frak(F) frak(G) frak(H) frak(I) frak(J) frak(K) frak(L) frak(M) frak(N) frak(O) frak(P) frak(Q) frak(R) frak(S) frak(T) frak(U) frak(V) frak(W) frak(X) frak(Y) frak(Z) frak(a) frak(b) frak(c) frak(d) frak(e) frak(f) frak(g) frak(h) frak(i) frak(j) frak(k) frak(l) frak(m) frak(n) frak(o) frak(p) frak(q) frak(r) frak(s) frak(t) frak(u) frak(v) frak(w) frak(x) frak(y) frak(z) \\
      bb(A) bb(B) bb(C) bb(D) bb(E) bb(F) bb(G) bb(H) bb(I) bb(J) bb(K) bb(L) bb(M) bb(N) bb(O) bb(P) bb(Q) bb(R) bb(S) bb(T) bb(U) bb(V) bb(W) bb(X) bb(Y) bb(Z) bb(a) bb(b) bb(c) bb(d) bb(e) bb(f) bb(g) bb(h) bb(i) bb(j) bb(k) bb(l) bb(m) bb(n) bb(o) bb(p) bb(q) bb(r) bb(s) bb(t) bb(u) bb(v) bb(w) bb(x) bb(y) bb(z) \\
      bb(0) bb(1) bb(2) bb(3) bb(4) bb(5) bb(6) bb(7) bb(8) bb(9) \\
    $
    = Functions
    $
      arrow(A) arrow(B) arrow(C) arrow(D) arrow(E) arrow(F) arrow(G) arrow(H) arrow(I) arrow(J) arrow(K) arrow(L) arrow(M) arrow(N) arrow(O) arrow(P) arrow(Q) arrow(R) arrow(S) arrow(T) arrow(U) arrow(V) arrow(W) arrow(X) arrow(Y) arrow(Z) arrow(a) arrow(b) arrow(c) arrow(d) arrow(e) arrow(f) arrow(g) arrow(h) arrow(i) arrow(j) arrow(k) arrow(l) arrow(m) arrow(n) arrow(o) arrow(p) arrow(q) arrow(r) arrow(s) arrow(t) arrow(u) arrow(v) arrow(w) arrow(x) arrow(y) arrow(z) arrow(0) arrow(1) arrow(2) arrow(3) arrow(4) arrow(5) arrow(6) arrow(7) arrow(8) arrow(9) \\
      arrow(alpha) arrow(Alpha) arrow(beta) arrow(Beta) arrow(gamma) arrow(Gamma) arrow(delta) arrow(Delta) arrow(epsilon) arrow(Epsilon) arrow(zeta) arrow(Zeta) arrow(eta) arrow(Eta) arrow(theta) arrow(Theta) arrow(iota) arrow(Iota) arrow(kappa) arrow(Kappa) arrow(lambda) arrow(Lambda) arrow(mu) arrow(Mu) arrow(nu) arrow(Nu) arrow(xi) arrow(Xi) arrow(omicron) arrow(Omicron) arrow(pi) arrow(Pi) arrow(rho) arrow(Rho) arrow(sigma) arrow(Sigma) arrow(tau) arrow(Tau) arrow(upsilon) arrow(Upsilon) arrow(phi) arrow(Phi) arrow(chi) arrow(Chi) arrow(psi) arrow(Psi) arrow(omega) arrow(Omega)
      tilde(A) tilde(B) tilde(C) tilde(D) tilde(E) tilde(F) tilde(G) tilde(H) tilde(I) tilde(J) tilde(K) tilde(L) tilde(M) tilde(N) tilde(O) tilde(P) tilde(Q) tilde(R) tilde(S) tilde(T) tilde(U) tilde(V) tilde(W) tilde(X) tilde(Y) tilde(Z) tilde(a) tilde(b) tilde(c) tilde(d) tilde(e) tilde(f) tilde(g) tilde(h) tilde(i) tilde(j) tilde(k) tilde(l) tilde(m) tilde(n) tilde(o) tilde(p) tilde(q) tilde(r) tilde(s) tilde(t) tilde(u) tilde(v) tilde(w) tilde(x) tilde(y) tilde(z) tilde(0) tilde(1) tilde(2) tilde(3) tilde(4) tilde(5) tilde(6) tilde(7) tilde(8) tilde(9) \\
      tilde(alpha) tilde(Alpha) tilde(beta) tilde(Beta) tilde(gamma) tilde(Gamma) tilde(delta) tilde(Delta) tilde(epsilon) tilde(Epsilon) tilde(zeta) tilde(Zeta) tilde(eta) tilde(Eta) tilde(theta) tilde(Theta) tilde(iota) tilde(Iota) tilde(kappa) tilde(Kappa) tilde(lambda) tilde(Lambda) tilde(mu) tilde(Mu) tilde(nu) tilde(Nu) tilde(xi) tilde(Xi) tilde(omicron) tilde(Omicron) tilde(pi) tilde(Pi) tilde(rho) tilde(Rho) tilde(sigma) tilde(Sigma) tilde(tau) tilde(Tau) tilde(upsilon) tilde(Upsilon) tilde(phi) tilde(Phi) tilde(chi) tilde(Chi) tilde(psi) tilde(Psi) tilde(omega) tilde(Omega) \\
      hat(A) hat(B) hat(C) hat(D) hat(E) hat(F) hat(G) hat(H) hat(I) hat(J) hat(K) hat(L) hat(M) hat(N) hat(O) hat(P) hat(Q) hat(R) hat(S) hat(T) hat(U) hat(V) hat(W) hat(X) hat(Y) hat(Z) hat(a) hat(b) hat(c) hat(d) hat(e) hat(f) hat(g) hat(h) hat(i) hat(j) hat(k) hat(l) hat(m) hat(n) hat(o) hat(p) hat(q) hat(r) hat(s) hat(t) hat(u) hat(v) hat(w) hat(x) hat(y) hat(z) hat(0) hat(1) hat(2) hat(3) hat(4) hat(5) hat(6) hat(7) hat(8) hat(9) \\
      hat(alpha) hat(Alpha) hat(beta) hat(Beta) hat(gamma) hat(Gamma) hat(delta) hat(Delta) hat(epsilon) hat(Epsilon) hat(zeta) hat(Zeta) hat(eta) hat(Eta) hat(theta) hat(Theta) hat(iota) hat(Iota) hat(kappa) hat(Kappa) hat(lambda) hat(Lambda) hat(mu) hat(Mu) hat(nu) hat(Nu) hat(xi) hat(Xi) hat(omicron) hat(Omicron) hat(pi) hat(Pi) hat(rho) hat(Rho) hat(sigma) hat(Sigma) hat(tau) hat(Tau) hat(upsilon) hat(Upsilon) hat(phi) hat(Phi) hat(chi) hat(Chi) hat(psi) hat(Psi) hat(omega) hat(Omega) \\
      dot(A) dot(B) dot(C) dot(D) dot(E) dot(F) dot(G) dot(H) dot(I) dot(J) dot(K) dot(L) dot(M) dot(N) dot(O) dot(P) dot(Q) dot(R) dot(S) dot(T) dot(U) dot(V) dot(W) dot(X) dot(Y) dot(Z) dot(a) dot(b) dot(c) dot(d) dot(e) dot(f) dot(g) dot(h) dot(i) dot(j) dot(k) dot(l) dot(m) dot(n) dot(o) dot(p) dot(q) dot(r) dot(s) dot(t) dot(u) dot(v) dot(w) dot(x) dot(y) dot(z) dot(0) dot(1) dot(2) dot(3) dot(4) dot(5) dot(6) dot(7) dot(8) dot(9) \\
      dot(alpha) dot(Alpha) dot(beta) dot(Beta) dot(gamma) dot(Gamma) dot(delta) dot(Delta) dot(epsilon) dot(Epsilon) dot(zeta) dot(Zeta) dot(eta) dot(Eta) dot(theta) dot(Theta) dot(iota) dot(Iota) dot(kappa) dot(Kappa) dot(lambda) dot(Lambda) dot(mu) dot(Mu) dot(nu) dot(Nu) dot(xi) dot(Xi) dot(omicron) dot(Omicron) dot(pi) dot(Pi) dot(rho) dot(Rho) dot(sigma) dot(Sigma) dot(tau) dot(Tau) dot(upsilon) dot(Upsilon) dot(phi) dot(Phi) dot(chi) dot(Chi) dot(psi) dot(Psi) dot(omega) dot(Omega) \\
    $
    $
      dot.double(A) dot.double(B) dot.double(C) dot.double(D) dot.double(E) dot.double(F) dot.double(G) dot.double(H) dot.double(I) dot.double(J) dot.double(K) dot.double(L) dot.double(M) dot.double(N) dot.double(O) dot.double(P) dot.double(Q) dot.double(R) dot.double(S) dot.double(T) dot.double(U) dot.double(V) dot.double(W) dot.double(X) dot.double(Y) dot.double(Z) dot.double(a) dot.double(b) dot.double(c) dot.double(d) dot.double(e) dot.double(f) dot.double(g) dot.double(h) dot.double(i) dot.double(j) dot.double(k) dot.double(l) dot.double(m) dot.double(n) dot.double(o) dot.double(p) dot.double(q) dot.double(r) dot.double(s) dot.double(t) dot.double(u) dot.double(v) dot.double(w) dot.double(x) dot.double(y) dot.double(z) dot.double(0) dot.double(1) dot.double(2) dot.double(3) dot.double(4) dot.double(5) dot.double(6) dot.double(7) dot.double(8) dot.double(9) \\
      dot.double(alpha) dot.double(Alpha) dot.double(beta) dot.double(Beta) dot.double(gamma) dot.double(Gamma) dot.double(delta) dot.double(Delta) dot.double(epsilon) dot.double(Epsilon) dot.double(zeta) dot.double(Zeta) dot.double(eta) dot.double(Eta) dot.double(theta) dot.double(Theta) dot.double(iota) dot.double(Iota) dot.double(kappa) dot.double(Kappa) dot.double(lambda) dot.double(Lambda) dot.double(mu) dot.double(Mu) dot.double(nu) dot.double(Nu) dot.double(xi) dot.double(Xi) dot.double(omicron) dot.double(Omicron) dot.double(pi) dot.double(Pi) dot.double(rho) dot.double(Rho) dot.double(sigma) dot.double(Sigma) dot.double(tau) dot.double(Tau) dot.double(upsilon) dot.double(Upsilon) dot.double(phi) dot.double(Phi) dot.double(chi) dot.double(Chi) dot.double(psi) dot.double(Psi) dot.double(omega) dot.double(Omega) \\
      dot.triple(A) dot.triple(B) dot.triple(C) dot.triple(D) dot.triple(E) dot.triple(F) dot.triple(G) dot.triple(H) dot.triple(I) dot.triple(J) dot.triple(K) dot.triple(L) dot.triple(M) dot.triple(N) dot.triple(O) dot.triple(P) dot.triple(Q) dot.triple(R) dot.triple(S) dot.triple(T) dot.triple(U) dot.triple(V) dot.triple(W) dot.triple(X) dot.triple(Y) dot.triple(Z) dot.triple(a) dot.triple(b) dot.triple(c) dot.triple(d) dot.triple(e) dot.triple(f) dot.triple(g) dot.triple(h) dot.triple(i) dot.triple(j) dot.triple(k) dot.triple(l) dot.triple(m) dot.triple(n) dot.triple(o) dot.triple(p) dot.triple(q) dot.triple(r) dot.triple(s) dot.triple(t) dot.triple(u) dot.triple(v) dot.triple(w) dot.triple(x) dot.triple(y) dot.triple(z) dot.triple(0) dot.triple(1) dot.triple(2) dot.triple(3) dot.triple(4) dot.triple(5) dot.triple(6) dot.triple(7) dot.triple(8) dot.triple(9) \\
      dot.triple(alpha) dot.triple(Alpha) dot.triple(beta) dot.triple(Beta) dot.triple(gamma) dot.triple(Gamma) dot.triple(delta) dot.triple(Delta) dot.triple(epsilon) dot.triple(Epsilon) dot.triple(zeta) dot.triple(Zeta) dot.triple(eta) dot.triple(Eta) dot.triple(theta) dot.triple(Theta) dot.triple(iota) dot.triple(Iota) dot.triple(kappa) dot.triple(Kappa) dot.triple(lambda) dot.triple(Lambda) dot.triple(mu) dot.triple(Mu) dot.triple(nu) dot.triple(Nu) dot.triple(xi) dot.triple(Xi) dot.triple(omicron) dot.triple(Omicron) dot.triple(pi) dot.triple(Pi) dot.triple(rho) dot.triple(Rho) dot.triple(sigma) dot.triple(Sigma) dot.triple(tau) dot.triple(Tau) dot.triple(upsilon) dot.triple(Upsilon) dot.triple(phi) dot.triple(Phi) dot.triple(chi) dot.triple(Chi) dot.triple(psi) dot.triple(Psi) dot.triple(omega) dot.triple(Omega) \\
      dot.quad(A) dot.quad(B) dot.quad(C) dot.quad(D) dot.quad(E) dot.quad(F) dot.quad(G) dot.quad(H) dot.quad(I) dot.quad(J) dot.quad(K) dot.quad(L) dot.quad(M) dot.quad(N) dot.quad(O) dot.quad(P) dot.quad(Q) dot.quad(R) dot.quad(S) dot.quad(T) dot.quad(U) dot.quad(V) dot.quad(W) dot.quad(X) dot.quad(Y) dot.quad(Z) dot.quad(a) dot.quad(b) dot.quad(c) dot.quad(d) dot.quad(e) dot.quad(f) dot.quad(g) dot.quad(h) dot.quad(i) dot.quad(j) dot.quad(k) dot.quad(l) dot.quad(m) dot.quad(n) dot.quad(o) dot.quad(p) dot.quad(q) dot.quad(r) dot.quad(s) dot.quad(t) dot.quad(u) dot.quad(v) dot.quad(w) dot.quad(x) dot.quad(y) dot.quad(z) dot.quad(0) dot.quad(1) dot.quad(2) dot.quad(3) dot.quad(4) dot.quad(5) dot.quad(6) dot.quad(7) dot.quad(8) dot.quad(9) \\
      dot.quad(alpha) dot.quad(Alpha) dot.quad(beta) dot.quad(Beta) dot.quad(gamma) dot.quad(Gamma) dot.quad(delta) dot.quad(Delta) dot.quad(epsilon) dot.quad(Epsilon) dot.quad(zeta) dot.quad(Zeta) dot.quad(eta) dot.quad(Eta) dot.quad(theta) dot.quad(Theta) dot.quad(iota) dot.quad(Iota) dot.quad(kappa) dot.quad(Kappa) dot.quad(lambda) dot.quad(Lambda) dot.quad(mu) dot.quad(Mu) dot.quad(nu) dot.quad(Nu) dot.quad(xi) dot.quad(Xi) dot.quad(omicron) dot.quad(Omicron) dot.quad(pi) dot.quad(Pi) dot.quad(rho) dot.quad(Rho) dot.quad(sigma) dot.quad(Sigma) dot.quad(tau) dot.quad(Tau) dot.quad(upsilon) dot.quad(Upsilon) dot.quad(phi) dot.quad(Phi) dot.quad(chi) dot.quad(Chi) dot.quad(psi) dot.quad(Psi) dot.quad(omega) dot.quad(Omega) \\
      overline(A) overline(B) overline(C) overline(D) overline(E) overline(F) overline(G) overline(H) overline(I) overline(J) overline(K) overline(L) overline(M) overline(N) overline(O) overline(P) overline(Q) overline(R) overline(S) overline(T) overline(U) overline(V) overline(W) overline(X) overline(Y) overline(Z) overline(a) overline(b) overline(c) overline(d) overline(e) overline(f) overline(g) overline(h) overline(i) overline(j) overline(k) overline(l) overline(m) overline(n) overline(o) overline(p) overline(q) overline(r) overline(s) overline(t) overline(u) overline(v) overline(w) overline(x) overline(y) overline(z) overline(0) overline(1) overline(2) overline(3) overline(4) overline(5) overline(6) overline(7) overline(8) overline(9) \\
      overline(alpha) overline(Alpha) overline(beta) overline(Beta) overline(gamma) overline(Gamma) overline(delta) overline(Delta) overline(epsilon) overline(Epsilon) overline(zeta) overline(Zeta) overline(eta) overline(Eta) overline(theta) overline(Theta) overline(iota) overline(Iota) overline(kappa) overline(Kappa) overline(lambda) overline(Lambda) overline(mu) overline(Mu) overline(nu) overline(Nu) overline(xi) overline(Xi) overline(omicron) overline(Omicron) overline(pi) overline(Pi) overline(rho) overline(Rho) overline(sigma) overline(Sigma) overline(tau) overline(Tau) overline(upsilon) overline(Upsilon) overline(phi) overline(Phi) overline(chi) overline(Chi) overline(psi) overline(Psi) overline(omega) overline(Omega) \\
      sqrt(A) sqrt(B) sqrt(C) sqrt(D) sqrt(E) sqrt(F) sqrt(G) sqrt(H) sqrt(I) sqrt(J) sqrt(K) sqrt(L) sqrt(M) sqrt(N) sqrt(O) sqrt(P) sqrt(Q) sqrt(R) sqrt(S) sqrt(T) sqrt(U) sqrt(V) sqrt(W) sqrt(X) sqrt(Y) sqrt(Z) sqrt(a) sqrt(b) sqrt(c) sqrt(d) sqrt(e) sqrt(f) sqrt(g) sqrt(h) sqrt(i) sqrt(j) sqrt(k) sqrt(l) sqrt(m) sqrt(n) sqrt(o) sqrt(p) sqrt(q) sqrt(r) sqrt(s) sqrt(t) sqrt(u) sqrt(v) sqrt(w) sqrt(x) sqrt(y) sqrt(z) sqrt(0) sqrt(1) sqrt(2) sqrt(3) sqrt(4) sqrt(5) sqrt(6) sqrt(7) sqrt(8) sqrt(9) \\
      sqrt(alpha) sqrt(Alpha) sqrt(beta) sqrt(Beta) sqrt(gamma) sqrt(Gamma) sqrt(delta) sqrt(Delta) sqrt(epsilon) sqrt(Epsilon) sqrt(zeta) sqrt(Zeta) sqrt(eta) sqrt(Eta) sqrt(theta) sqrt(Theta) sqrt(iota) sqrt(Iota) sqrt(kappa) sqrt(Kappa) sqrt(lambda) sqrt(Lambda) sqrt(mu) sqrt(Mu) sqrt(nu) sqrt(Nu) sqrt(xi) sqrt(Xi) sqrt(omicron) sqrt(Omicron) sqrt(pi) sqrt(Pi) sqrt(rho) sqrt(Rho) sqrt(sigma) sqrt(Sigma) sqrt(tau) sqrt(Tau) sqrt(upsilon) sqrt(Upsilon) sqrt(phi) sqrt(Phi) sqrt(chi) sqrt(Chi) sqrt(psi) sqrt(Psi) sqrt(omega) sqrt(Omega) \\
      abs(A) abs(B) abs(C) abs(D) abs(E) abs(F) abs(G) abs(H) abs(I) abs(J) abs(K) abs(L) abs(M) abs(N) abs(O) abs(P) abs(Q) abs(R) abs(S) abs(T) abs(U) abs(V) abs(W) abs(X) abs(Y) abs(Z) abs(a) abs(b) abs(c) abs(d) abs(e) abs(f) abs(g) abs(h) abs(i) abs(j) abs(k) abs(l) abs(m) abs(n) abs(o) abs(p) abs(q) abs(r) abs(s) abs(t) abs(u) abs(v) abs(w) abs(x) abs(y) abs(z) abs(0) abs(1) abs(2) abs(3) abs(4) abs(5) abs(6) abs(7) abs(8) abs(9) abs(999) \\
      abs(alpha) abs(Alpha) abs(beta) abs(Beta) abs(gamma) abs(Gamma) abs(delta) abs(Delta) abs(epsilon) abs(Epsilon) abs(zeta) abs(Zeta) abs(eta) abs(Eta) abs(theta) abs(Theta) abs(iota) abs(Iota) abs(kappa) abs(Kappa) abs(lambda) abs(Lambda) abs(mu) abs(Mu) abs(nu) abs(Nu) abs(xi) abs(Xi) abs(omicron) abs(Omicron) abs(pi) abs(Pi) abs(rho) abs(Rho) abs(sigma) abs(Sigma) abs(tau) abs(Tau) abs(upsilon) abs(Upsilon) abs(phi) abs(Phi) abs(chi) abs(Chi) abs(psi) abs(Psi) abs(omega) abs(Omega) \\
      norm(A) norm(B) norm(C) norm(D) norm(E) norm(F) norm(G) norm(H) norm(I) norm(J) norm(K) norm(L) norm(M) norm(N) norm(O) norm(P) norm(Q) norm(R) norm(S) norm(T) norm(U) norm(V) norm(W) norm(X) norm(Y) norm(Z) norm(a) norm(b) norm(c) norm(d) norm(e) norm(f) norm(g) norm(h) norm(i) norm(j) norm(k) norm(l) norm(m) norm(n) norm(o) norm(p) norm(q) norm(r) norm(s) norm(t) norm(u) \\
    $
    = Complex composition
    $
      abs(\"very long abs\" 23324) norm(a_a_a_a_a^a^a^a)
      alpha(alpha)
      arrow(alpha_i) abs(f^0) dot(x_2)
      .integral
      sqrt(a^alpha) sqrt(a)^alpha overline(a^i) sqrt(a_i) beta^sqrt(alpha^zeta) beta^abs(alpha^zeta) beta^tilde(alpha)
      sigma(x)
      tilde(x) tilde(tilde)
      arrow(e)
      integral.
    $
    
    = All symbols
    $
      wj: \"wjoin,\"\\
      zwj: \"zwj,\"\\
      zwnj: \"zwnj,\"\\
      zws: \"zwsp,\"\\
      space: \"␣,\"\\
      space.nobreak: \"nbsp,\"\\
      space.en: \"ensp,\"\\
      space.quad: \"emsp,\"\\
      space.third: \"⅓emsp,\"\\
      space.quarter: \"¼emsp,\"\\
      space.sixth: \"⅙emsp,\"\\
      space.med: \"mmsp,\"\\
      space.fig: \"numsp,\"\\
      space.punct: \"puncsp,\"\\
      space.thin: \"thinsp,\"\\
      space.hair: \"hairsp,\"\\
      paren.l: \"(,\"\\
      paren.r: \"),\"\\
      paren.t: \"⏜,\"\\
      paren.b: \"⏝,\"\\
      brace.l: \"{,\"\\
      brace.r: \"},\"\\
      brace.t: \"⏞,\"\\
      brace.b: \"⏟,\"\\
      bracket.l: \"[,\"\\
      bracket.l.double: \"⟦,\"\\
      bracket.r: \"],\"\\
      bracket.r.double: \"⟧,\"\\
      bracket.t: \"⎴,\"\\
      bracket.b: \"⎵,\"\\
      turtle.l: \"〔,\"\\
      turtle.r: \"〕,\"\\
      turtle.t: \"⏠,\"\\
      turtle.b : \"⏡,\"\\
      bar.v: \"|,\"\\
      bar.v.double: \"‖,\"\\
      bar.v.triple: \"⦀,\"\\
      bar.v.broken: \"¦,\"\\
      bar.v.circle: \"⦶,\"\\
      bar.h: \"―,\"\\
      fence.l: \"⧘,\"\\
      fence.l.double: \"⧚,\"\\
      fence.r: \"⧙,\"\\
      fence.r.double: \"⧛,\"\\
      fence.dotted: \"⦙,\"\\
      angle: \"∠,\"\\
      angle.l: \"⟨,\"\\
      angle.r: \"⟩,\"\\
      angle.l.double: \"《,\"\\
      angle.r.double: \"》,\"\\
      angle.acute: \"⦟,\"\\
      angle.arc: \"∡,\"\\
      angle.arc.rev: \"⦛,\"\\
      angle.rev: \"⦣,\"\\
      angle.right: \"∟,\"\\
      angle.right.rev: \"⯾,\"\\
      angle.right.arc: \"⊾,\"\\
      angle.right.dot: \"⦝,\"\\
      angle.right.sq: \"⦜,\"\\
      angle.spatial: \"⟀,\"\\
      angle.spheric: \"∢,\"\\
      angle.spheric.rev: \"⦠,\"\\
      angle.spheric.top: \"⦡,\"\\
      amp: \"&,\"\\
      amp.inv: \"⅋,\"\\
      ast.op: \"∗,\"\\
      ast.basic: \"\"\\*,\\
      ast.low: \"⁎,\"\\
      ast.double: \"⁑,\"\\
      ast.triple: \"⁂,\"\\
      ast.small: \"﹡,\"\\
      ast.circle: \"⊛,\"\\
      ast.square: \"⧆,\"\\
      at: \"\"\\@,\\
      backslash: \"\"\\\\,\\
      backslash.circle: \"⦸,\"\\
      backslash.not: \"⧷,\"\\
      co: \"℅,\"\\
      colon: \":,\"\\
      colon.eq: \"≔,\"\\
      colon.double.eq: \"⩴,\"\\
      comma: \", \"\\,\\
      dagger: \"†,\"\\
      dagger.double: \"‡,\"\\
      dash.en: \"–,\"\\
      dash.em: \"—,\"\\
      dash.fig: \"‒,\"\\
      dash.wave: \"〜,\"\\
      dash.colon: \"∹,\"\\
      dash.circle: \"⊝,\"\\
      dash.wave.double: \"〰,\"\\
      dot.op: \"⋅,\"\\
      dot.basic: \".,\"\\
      dot.c: \"·,\"\\
      dot.circle: \"⊙,\"\\
      dot.circle.big: \"⨀,\"\\
      dot.square: \"⊡,\"\\
      dot.double: \"¨,\"\\
      dot.triple: \"⃛,\"\\
      dot.quad: \"⃜,\"\\
      excl: \"!,\"\\
      excl.double: \"‼,\"\\
      excl.inv: \"¡,\"\\
      excl.quest: \"⁉,\"\\
      quest: \"?,\"\\
      quest.double: \"⁇,\"\\
      quest.excl: \"⁈,\"\\
      quest.inv: \"¿,\"\\
      interrobang: \"‽,\"\\
      hash: \"\"\\#,\\
      hyph: \"‐,\"\\
      hyph.minus: \"-,\"\\
      hyph.nobreak: \"‑,\"\\
      hyph.point: \"‧,\"\\
      hyph.soft: \"shy,\"\\
      percent: \"%,\"\\
      copyright: \"©,\"\\
      copyright.sound: \"℗,\"\\
      permille: \"‰,\"\\
      pilcrow: \"¶,\"\\
      pilcrow.rev: \"⁋,\"\\
      section: \"§,\"\\
      semi: \";,\"\\
      semi.rev: \"⁏,\"\\
      slash: \"/,\"\\
      slash.double: \"⫽,\"\\
      slash.triple: \"⫻,\"\\
      slash.big: \"⧸,\"\\
      dots.h.c: \"⋯,\"\\
      dots.h: \"…,\"\\
      dots.v: \"⋮,\"\\
      dots.down: \"⋱,\"\\
      dots.up: \"⋰,\"\\
      tilde.op: \"∼,\"\\
      tilde.basic: \"~,\"\\
      tilde.eq: \"≃,\"\\
      tilde.eq.not: \"≄,\"\\
      tilde.eq.rev: \"⋍,\"\\
      tilde.equiv: \"≅,\"\\
      tilde.equiv.not: \"≇,\"\\
      tilde.nequiv: \"≆,\"\\
      tilde.not: \"≁,\"\\
      tilde.rev: \"∽,\"\\
      tilde.rev.equiv: \"≌,\"\\
      tilde.triple: \"≋,\"\\
      acute: \"´,\"\\
      acute.double: \"˝,\"\\
      breve: \"˘,\"\\
      caret: \"‸,\"\\
      caron: \"ˇ,\"\\
      hat: \"^,\"\\
      diaer: \"¨,\"\\
      grave: \"\"\\`,\\
      macron: \"¯,\"\\
      quote.double: \"\"\\,\\
      quote.single: \"',\"\\
      quote.l.double: \"“,\"\\
      quote.l.single: \"‘,\"\\
      quote.r.double: \"”,\"\\
      quote.r.single: \"’,\"\\
      quote.angle.l.double: \"«,\"\\
      quote.angle.l.single: \"‹,\"\\
      quote.angle.r.double: \"»,\"\\
      quote.angle.r.single: \"›,\"\\
      quote.high.double: \"‟,\"\\
      quote.high.single: \"‛,\"\\
      quote.low.double: \"„,\"\\
      quote.low.single: \"‚,\"\\
      prime: \"′,\"\\
      prime.rev: \"‵,\"\\
      prime.double: \"″,\"\\
      prime.double.rev: \"‶,\"\\
      prime.triple: \"‴,\"\\
      prime.triple.rev: \"‷,\"\\
      prime.quad: \"⁗,\"\\
      plus: \"+,\"\\
      plus.circle: \"⊕,\"\\
      plus.circle.arrow: \"⟴,\"\\
      plus.circle.big: \"⨁,\"\\
      plus.dot: \"∔,\"\\
      plus.minus: \"±,\"\\
      plus.small: \"﹢,\"\\
      plus.square: \"⊞,\"\\
      plus.triangle: \"⨹,\"\\
      minus: \"−,\"\\
      minus.circle: \"⊖,\"\\
      minus.dot: \"∸,\"\\
      minus.plus: \"∓,\"\\
      minus.square: \"⊟,\"\\
      minus.tilde: \"≂,\"\\
      minus.triangle: \"⨺,\"\\
      div: \"÷,\"\\
      div.circle: \"⨸,\"\\
      times: \"×,\"\\
      times.big: \"⨉,\"\\
      times.circle: \"⊗,\"\\
      times.circle.big: \"⨂,\"\\
      times.div: \"⋇,\"\\
      times.three.l: \"⋋,\"\\
      times.three.r: \"⋌,\"\\
      times.l: \"⋉,\"\\
      times.r: \"⋊,\"\\
      times.square: \"⊠,\"\\
      times.triangle: \"⨻,\"\\
      ratio: \"∶,\"\\
      eq: \"=,\"\\
      eq.star: \"≛,\"\\
      eq.circle: \"⊜,\"\\
      eq.colon: \"≕,\"\\
      eq.def: \"≝,\"\\
      eq.delta: \"≜,\"\\
      eq.equi: \"≚,\"\\
      eq.est: \"≙,\"\\
      eq.gt: \"⋝,\"\\
      eq.lt: \"⋜,\"\\
      eq.m: \"≞,\"\\
      eq.not: \"≠,\"\\
      eq.prec: \"⋞,\"\\
      eq.quest: \"≟,\"\\
      eq.small: \"﹦,\"\\
      eq.succ: \"⋟,\"\\
      eq.triple: \"≡,\"\\
      eq.quad: \"≣,\"\\
      gt: \">,\"\\
      gt.circle: \"⧁,\"\\
      gt.curly: \"≻,\"\\
      gt.curly.approx: \"⪸,\"\\
      gt.curly.double: \"⪼,\"\\
      gt.curly.eq: \"≽,\"\\
      gt.curly.eq.not: \"⋡,\"\\
      gt.curly.equiv: \"⪴,\"\\
      gt.curly.napprox: \"⪺,\"\\
      gt.curly.nequiv: \"⪶,\"\\
      gt.curly.not: \"⊁,\"\\
      gt.curly.ntilde: \"⋩,\"\\
      gt.curly.tilde: \"≿,\"\\
      gt.dot: \"⋗,\"\\
      gt.double: \"≫,\"\\
      gt.eq: \"≥,\"\\
      gt.eq.slant: \"⩾,\"\\
      gt.eq.lt: \"⋛,\"\\
      gt.eq.not: \"≱,\"\\
      gt.equiv: \"≧,\"\\
      gt.lt: \"≷,\"\\
      gt.lt.not: \"≹,\"\\
      gt.nequiv: \"≩,\"\\
      gt.not: \"≯,\"\\
      gt.ntilde: \"⋧,\"\\
      gt.small: \"﹥,\"\\
      gt.tilde: \"≳,\"\\
      gt.tilde.not: \"≵,\"\\
      gt.tri: \"⊳,\"\\
      gt.tri.eq: \"⊵,\"\\
      gt.tri.eq.not: \"⋭,\"\\
      gt.tri.not: \"⋫,\"\\
      gt.triple: \"⋙,\"\\
      gt.triple.nested: \"⫸,\"\\
      lt: \"<,\"\\
      lt.circle: \"⧀,\"\\
      lt.curly: \"≺,\"\\
      lt.curly.approx: \"⪷,\"\\
      lt.curly.double: \"⪻,\"\\
      lt.curly.eq: \"≼,\"\\
      lt.curly.eq.not: \"⋠,\"\\
      lt.curly.equiv: \"⪳,\"\\
      lt.curly.napprox: \"⪹,\"\\
      lt.curly.nequiv: \"⪵,\"\\
      lt.curly.not: \"⊀,\"\\
      lt.curly.ntilde: \"⋨,\"\\
      lt.curly.tilde: \"≾,\"\\
      lt.dot: \"⋖,\"\\
      lt.double: \"≪,\"\\
      lt.eq: \"≤,\"\\
      lt.eq.slant: \"⩽,\"\\
      lt.eq.gt: \"⋚,\"\\
      lt.eq.not: \"≰,\"\\
      lt.equiv: \"≦,\"\\
      lt.gt: \"≶,\"\\
      lt.gt.not: \"≸,\"\\
      lt.nequiv: \"≨,\"\\
      lt.not: \"≮,\"\\
      lt.ntilde: \"⋦,\"\\
      lt.small: \"﹤,\"\\
      lt.tilde: \"≲,\"\\
      lt.tilde.not: \"≴,\"\\
      lt.tri: \"⊲,\"\\
      lt.tri.eq: \"⊴,\"\\
      lt.tri.eq.not: \"⋬,\"\\
      lt.tri.not: \"⋪,\"\\
      lt.triple: \"⋘,\"\\
      lt.triple.nested: \"⫷,\"\\
      approx: \"≈,\"\\
      approx.eq: \"≊,\"\\
      approx.not: \"≉,\"\\
      prec: \"≺,\"\\
      prec.approx: \"⪷,\"\\
      prec.double: \"⪻,\"\\
      prec.eq: \"≼,\"\\
      prec.eq.not: \"⋠,\"\\
      prec.equiv: \"⪳,\"\\
      prec.napprox: \"⪹,\"\\
      prec.nequiv: \"⪵,\"\\
      prec.not: \"⊀,\"\\
      prec.ntilde: \"⋨,\"\\
      prec.tilde: \"≾,\"\\
      succ: \"≻,\"\\
      succ.approx: \"⪸,\"\\
      succ.double: \"⪼,\"\\
      succ.eq: \"≽,\"\\
      succ.eq.not: \"⋡,\"\\
      succ.equiv: \"⪴,\"\\
      succ.napprox: \"⪺,\"\\
      succ.nequiv: \"⪶,\"\\
      succ.not: \"⊁,\"\\
      succ.ntilde: \"⋩,\"\\
      succ.tilde: \"≿,\"\\
      equiv: \"≡,\"\\
      equiv.not: \"≢,\"\\
      prop: \"∝,\"\\
      emptyset: \"∅,\"\\
      emptyset.rev: \"⦰,\"\\
      nothing: \"∅,\"\\
      nothing.rev: \"⦰,\"\\
      without: \"∖,\"\\
      complement: \"∁,\"\\
      in: \"∈,\"\\
      in.not: \"∉,\"\\
      in.rev: \"∋,\"\\
      in.rev.not: \"∌,\"\\
      in.rev.small: \"∍,\"\\
      in.small: \"∊,\"\\
      subset: \"⊂,\"\\
      subset.dot: \"⪽,\"\\
      subset.double: \"⋐,\"\\
      subset.eq: \"⊆,\"\\
      subset.eq.not: \"⊈,\"\\
      subset.eq.sq: \"⊑,\"\\
      subset.eq.sq.not: \"⋢,\"\\
      subset.neq: \"⊊,\"\\
      subset.not: \"⊄,\"\\
      subset.sq: \"⊏,\"\\
      subset.sq.neq: \"⋤,\"\\
      supset: \"⊃,\"\\
      supset.dot: \"⪾,\"\\
      supset.double: \"⋑,\"\\
      supset.eq: \"⊇,\"\\
      supset.eq.not: \"⊉,\"\\
      supset.eq.sq: \"⊒,\"\\
      supset.eq.sq.not: \"⋣,\"\\
      supset.neq: \"⊋,\"\\
      supset.not: \"⊅,\"\\
      supset.sq: \"⊐,\"\\
      supset.sq.neq: \"⋥,\"\\
      union: \"∪,\"\\
      union.arrow: \"⊌,\"\\
      union.big: \"⋃,\"\\
      union.dot: \"⊍,\"\\
      union.dot.big: \"⨃,\"\\
      union.double: \"⋓,\"\\
      union.minus: \"⩁,\"\\
      union.or: \"⩅,\"\\
      union.plus: \"⊎,\"\\
      union.plus.big: \"⨄,\"\\
      union.sq: \"⊔,\"\\
      union.sq.big: \"⨆,\"\\
      union.sq.double: \"⩏,\"\\
      sect: \"∩,\"\\
      sect.and: \"⩄,\"\\
      sect.big: \"⋂,\"\\
      sect.dot: \"⩀,\"\\
      sect.double: \"⋒,\"\\
      sect.sq: \"⊓,\"\\
      sect.sq.big: \"⨅,\"\\
      sect.sq.double: \"⩎,\"\\
      infinity: \"∞,\"\\
      oo: \"∞,\"\\
      diff: \"∂,\"\\
      nabla: \"∇,\"\\
      sum: \"∑,\"\\
      sum.integral: \"⨋,\"\\
      product: \"∏,\"\\
      product.co: \"∐,\"\\
      integral: \"∫,\"\\
      integral.arrow.hook: \"⨗,\"\\
      integral.ccw: \"⨑,\"\\
      integral.cont: \"∮,\"\\
      integral.cont.ccw: \"∳,\"\\
      integral.cont.cw: \"∲,\"\\
      integral.cw: \"∱,\"\\
      integral.dash: \"⨍,\"\\
      integral.dash.double: \"⨎,\"\\
      integral.double: \"∬,\"\\
      integral.quad: \"⨌,\"\\
      integral.sect: \"⨙,\"\\
      integral.slash: \"⨏,\"\\
      integral.square: \"⨖,\"\\
      integral.surf: \"∯,\"\\
      integral.times: \"⨘,\"\\
      integral.triple: \"∭,\"\\
      integral.union: \"⨚,\"\\
      integral.vol: \"∰,\"\\
      laplace: \"∆,\"\\
      forall: \"∀,\"\\
      exists: \"∃,\"\\
      exists.not: \"∄,\"\\
      top: \"⊤,\"\\
      bot: \"⊥,\"\\
      not: \"¬,\"\\
      and: \"∧,\"\\
      and.big: \"⋀,\"\\
      and.curly: \"⋏,\"\\
      and.dot: \"⟑,\"\\
      and.double: \"⩓,\"\\
      or: \"∨,\"\\
      or.big: \"⋁,\"\\
      or.curly: \"⋎,\"\\
      or.dot: \"⟇,\"\\
      or.double: \"⩔,\"\\
      xor: \"⊕,\"\\
      xor.big: \"⨁,\"\\
      models: \"⊧,\"\\
      therefore: \"∴,\"\\
      because: \"∵,\"\\
      qed: \"∎,\"\\
      compose: \"∘,\"\\
      convolve: \"∗,\"\\
      multimap: \"⊸,\"\\
      divides: \"∣,\"\\
      divides.not: \"∤,\"\\
      wreath: \"≀,\"\\
      parallel: \"∥,\"\\
      parallel.circle: \"⦷,\"\\
      parallel.not: \"∦,\"\\
      perp: \"⟂,\"\\
      perp.circle: \"⦹,\"\\
      diameter: \"⌀,\"\\
      join: \"⨝,\"\\
      join.r: \"⟖,\"\\
      join.l: \"⟕,\"\\
      join.l.r: \"⟗,\"\\
      degree: \"°,\"\\
      degree.c: \"℃,\"\\
      degree.f: \"℉,\"\\
      smash: \"⨳,\"\\
      bitcoin: \"₿,\"\\
      dollar: \"\"\\$,\\
      euro: \"€,\"\\
      franc: \"₣,\"\\
      lira: \"₺,\"\\
      peso: \"₱,\"\\
      pound: \"£,\"\\
      ruble: \"₽,\"\\
      rupee: \"₹,\"\\
      won: \"₩,\"\\
      yen: \"¥,\"\\
      ballot: \"☐,\"\\
      ballot.x: \"☒,\"\\
      checkmark: \"✓,\"\\
      checkmark.light: \"🗸,\"\\
      floral: \"❦,\"\\
      floral.l: \"☙,\"\\
      floral.r: \"❧,\"\\
      notes.up: \"🎜,\"\\
      notes.down: \"🎝,\"\\
      refmark: \"※,\"\\
      servicemark: \"℠,\"\\
      maltese: \"✠,\"\\
      suit.club: \"♣,\"\\
      suit.diamond: \"♦,\"\\
      suit.heart: \"♥,\"\\
      suit.spade: \"♠,\"\\
      bullet: \"•,\"\\
      circle.stroked: \"○,\"\\
      circle.stroked.tiny: \"∘,\"\\
      circle.stroked.small: \"⚬,\"\\
      circle.stroked.big: \"◯,\"\\
      circle.filled: \"●,\"\\
      circle.filled.tiny: \"⦁,\"\\
      circle.filled.small: \"∙,\"\\
      circle.filled.big: \"⬤,\"\\
      circle.dotted: \"◌,\"\\
      circle.nested: \"⊚,\"\\
      ellipse.stroked.h: \"⬭,\"\\
      ellipse.stroked.v: \"⬯,\"\\
      ellipse.filled.h: \"⬬,\"\\
      ellipse.filled.v: \"⬮,\"\\
      triangle.stroked.r: \"▷,\"\\
      triangle.stroked.l: \"◁,\"\\
      triangle.stroked.t: \"△,\"\\
      triangle.stroked.b: \"▽,\"\\
      triangle.stroked.bl: \"◺,\"\\
      triangle.stroked.br: \"◿,\"\\
      triangle.stroked.tl: \"◸,\"\\
      triangle.stroked.tr: \"◹,\"\\
      triangle.stroked.small.r: \"▹,\"\\
      triangle.stroked.small.b: \"▿,\"\\
      triangle.stroked.small.l: \"◃,\"\\
      triangle.stroked.small.t: \"▵,\"\\
      triangle.stroked.rounded: \"🛆,\"\\
      triangle.stroked.nested: \"⟁,\"\\
      triangle.stroked.dot: \"◬,\"\\
      triangle.filled.r: \"▶,\"\\
      triangle.filled.l: \"◀,\"\\
      triangle.filled.t: \"▲,\"\\
      triangle.filled.b: \"▼,\"\\
      triangle.filled.bl: \"◣,\"\\
      triangle.filled.br: \"◢,\"\\
      triangle.filled.tl: \"◤,\"\\
      triangle.filled.tr: \"◥,\"\\
      triangle.filled.small.r: \"▸,\"\\
      triangle.filled.small.b: \"▾,\"\\
      triangle.filled.small.l: \"◂,\"\\
      triangle.filled.small.t: \"▴,\"\\
      square.stroked: \"□,\"\\
      square.stroked.tiny: \"▫,\"\\
      square.stroked.small: \"◽,\"\\
      square.stroked.medium: \"◻,\"\\
      square.stroked.big: \"⬜,\"\\
      square.stroked.dotted: \"⬚,\"\\
      square.stroked.rounded: \"▢,\"\\
      square.filled: \"■,\"\\
      square.filled.tiny: \"▪,\"\\
      square.filled.small: \"◾,\"\\
      square.filled.medium: \"◼,\"\\
      square.filled.big: \"⬛,\"\\
      rect.stroked.h: \"▭,\"\\
      rect.stroked.v: \"▯,\"\\
      rect.filled.h: \"▬,\"\\
      rect.filled.v: \"▮,\"\\
      penta.stroked: \"⬠,\"\\
      penta.filled: \"⬟,\"\\
      hexa.stroked: \"⬡,\"\\
      hexa.filled: \"⬢,\"\\
      diamond.stroked: \"◇,\"\\
      diamond.stroked.small: \"⋄,\"\\
      diamond.stroked.medium: \"⬦,\"\\
      diamond.stroked.dot: \"⟐,\"\\
      diamond.filled: \"◆,\"\\
      diamond.filled.medium: \"⬥,\"\\
      diamond.filled.small: \"⬩,\"\\
      lozenge.stroked: \"◊,\"\\
      lozenge.stroked.small: \"⬫,\"\\
      lozenge.stroked.medium: \"⬨,\"\\
      lozenge.filled: \"⧫,\"\\
      lozenge.filled.small: \"⬪,\"\\
      lozenge.filled.medium: \"⬧,\"\\
      star.op: \"⋆,\"\\
      star.stroked: \"★,\"\\
      star.filled: \"★,\"\\
      arrow.r: \"→,\"\\
      arrow.r.long.bar: \"⟼,\"\\
      arrow.r.bar: \"↦,\"\\
      arrow.r.curve: \"⤷,\"\\
      arrow.r.dashed: \"⇢,\"\\
      arrow.r.dotted: \"⤑,\"\\
      arrow.r.double: \"⇒,\"\\
      arrow.r.double.bar: \"⤇,\"\\
      arrow.r.double.long: \"⟹,\"\\
      arrow.r.double.long.bar: \"⟾,\"\\
      arrow.r.double.not: \"⇏,\"\\
      arrow.r.filled: \"➡,\"\\
      arrow.r.hook: \"↪,\"\\
      arrow.r.long: \"⟶,\"\\
      arrow.r.long.squiggly: \"⟿,\"\\
      arrow.r.loop: \"↬,\"\\
      arrow.r.not: \"↛,\"\\
      arrow.r.quad: \"⭆,\"\\
      arrow.r.squiggly: \"⇝,\"\\
      arrow.r.stop: \"⇥,\"\\
      arrow.r.stroked: \"⇨,\"\\
      arrow.r.tail: \"↣,\"\\
      arrow.r.triple: \"⇛,\"\\
      arrow.r.twohead.bar: \"⤅,\"\\
      arrow.r.twohead: \"↠,\"\\
      arrow.r.wave: \"↝,\"\\
      arrow.l: \"←,\"\\
      arrow.l.bar: \"↤,\"\\
      arrow.l.curve: \"⤶,\"\\
      arrow.l.dashed: \"⇠,\"\\
      arrow.l.dotted: \"⬸,\"\\
      arrow.l.double: \"⇐,\"\\
      arrow.l.double.bar: \"⤆,\"\\
      arrow.l.double.long: \"⟸,\"\\
      arrow.l.double.long.bar: \"⟽,\"\\
      arrow.l.double.not: \"⇍,\"\\
      arrow.l.filled: \"⬅,\"\\
      arrow.l.hook: \"↩,\"\\
      arrow.l.long: \"⟵,\"\\
      arrow.l.long.bar: \"⟻,\"\\
      arrow.l.long.squiggly: \"⬳,\"\\
      arrow.l.loop: \"↫,\"\\
      arrow.l.not: \"↚,\"\\
      arrow.l.quad: \"⭅,\"\\
      arrow.l.squiggly: \"⇜,\"\\
      arrow.l.stop: \"⇤,\"\\
      arrow.l.stroked: \"⇦,\"\\
      arrow.l.tail: \"↢,\"\\
      arrow.l.triple: \"⇚,\"\\
      arrow.l.twohead.bar: \"⬶,\"\\
      arrow.l.twohead: \"↞,\"\\
      arrow.l.wave: \"↜,\"\\
      arrow.t: \"↑,\"\\
      arrow.t.bar: \"↥,\"\\
      arrow.t.curve: \"⤴,\"\\
      arrow.t.dashed: \"⇡,\"\\
      arrow.t.double: \"⇑,\"\\
      arrow.t.filled: \"⬆,\"\\
      arrow.t.quad: \"⟰,\"\\
      arrow.t.stop: \"⤒,\"\\
      arrow.t.stroked: \"⇧,\"\\
      arrow.t.triple: \"⤊,\"\\
      arrow.t.twohead: \"↟,\"\\
      arrow.b: \"↓,\"\\
      arrow.b.bar: \"↧,\"\\
      arrow.b.curve: \"⤵,\"\\
      arrow.b.dashed: \"⇣,\"\\
      arrow.b.double: \"⇓,\"\\
      arrow.b.filled: \"⬇,\"\\
      arrow.b.quad: \"⟱,\"\\
      arrow.b.stop: \"⤓,\"\\
      arrow.b.stroked: \"⇩,\"\\
      arrow.b.triple: \"⤋,\"\\
      arrow.b.twohead: \"↡,\"\\
      arrow.l.r: \"↔,\"\\
      arrow.l.r.double: \"⇔,\"\\
      arrow.l.r.double.long: \"⟺,\"\\
      arrow.l.r.double.not: \"⇎,\"\\
      arrow.l.r.filled: \"⬌,\"\\
      arrow.l.r.long: \"⟷,\"\\
      arrow.l.r.not: \"↮,\"\\
      arrow.l.r.stroked: \"⬄,\"\\
      arrow.l.r.wave: \"↭,\"\\
      arrow.t.b: \"↕,\"\\
      arrow.t.b.double: \"⇕,\"\\
      arrow.t.b.filled: \"⬍,\"\\
      arrow.t.b.stroked: \"⇳,\"\\
      arrow.tr: \"↗,\"\\
      arrow.tr.double: \"⇗,\"\\
      arrow.tr.filled: \"⬈,\"\\
      arrow.tr.hook: \"⤤,\"\\
      arrow.tr.stroked: \"⬀,\"\\
      arrow.br: \"↘,\"\\
      arrow.br.double: \"⇘,\"\\
      arrow.br.filled: \"⬊,\"\\
      arrow.br.hook: \"⤥,\"\\
      arrow.br.stroked: \"⬂,\"\\
      arrow.tl: \"↖,\"\\
      arrow.tl.double: \"⇖,\"\\
      arrow.tl.filled: \"⬉,\"\\
      arrow.tl.hook: \"⤣,\"\\
      arrow.tl.stroked: \"⬁,\"\\
      arrow.bl: \"↙,\"\\
      arrow.bl.double: \"⇙,\"\\
      arrow.bl.filled: \"⬋,\"\\
      arrow.bl.hook: \"⤦,\"\\
      arrow.bl.stroked: \"⬃,\"\\
      arrow.tl.br: \"⤡,\"\\
      arrow.tr.bl: \"⤢,\"\\
      arrow.ccw: \"↺,\"\\
      arrow.ccw.half: \"↶,\"\\
      arrow.cw: \"↻,\"\\
      arrow.cw.half: \"↷,\"\\
      arrow.zigzag: \"↯,\"\\
      arrows.rr: \"⇉,\"\\
      arrows.ll: \"⇇,\"\\
      arrows.tt: \"⇈,\"\\
      arrows.bb: \"⇊,\"\\
      arrows.lr: \"⇆,\"\\
      arrows.lr.stop: \"↹,\"\\
      arrows.rl: \"⇄,\"\\
      arrows.tb: \"⇅,\"\\
      arrows.bt: \"⇵,\"\\
      arrows.rrr: \"⇶,\"\\
      arrows.lll: \"⬱,\"\\
      arrowhead.t: \"⌃,\"\\
      arrowhead.b: \"⌄,\"\\
      harpoon.rt: \"⇀,\"\\
      harpoon.rt.bar: \"⥛,\"\\
      harpoon.rt.stop: \"⥓,\"\\
      harpoon.rb: \"⇁,\"\\
      harpoon.rb.bar: \"⥟,\"\\
      harpoon.rb.stop: \"⥗,\"\\
      harpoon.lt: \"↼,\"\\
      harpoon.lt.bar: \"⥚,\"\\
      harpoon.lt.stop: \"⥒,\"\\
      harpoon.lb: \"↽,\"\\
      harpoon.lb.bar: \"⥞,\"\\
      harpoon.lb.stop: \"⥖,\"\\
      harpoon.tl: \"↿,\"\\
      harpoon.tl.bar: \"⥠,\"\\
      harpoon.tl.stop: \"⥘,\"\\
      harpoon.tr: \"↾,\"\\
      harpoon.tr.bar: \"⥜,\"\\
      harpoon.tr.stop: \"⥔,\"\\
      harpoon.bl: \"⇃,\"\\
      harpoon.bl.bar: \"⥡,\"\\
      harpoon.bl.stop: \"⥙,\"\\
      harpoon.br: \"⇂,\"\\
      harpoon.br.bar: \"⥝,\"\\
      harpoon.br.stop: \"⥕,\"\\
      harpoon.lt.rt: \"⥎,\"\\
      harpoon.lb.rb: \"⥐,\"\\
      harpoon.lb.rt: \"⥋,\"\\
      harpoon.lt.rb: \"⥊,\"\\
      harpoon.tl.bl: \"⥑,\"\\
      harpoon.tr.br: \"⥏,\"\\
      harpoon.tl.br: \"⥍,\"\\
      harpoon.tr.bl: \"⥌,\"\\
      harpoons.rtrb: \"⥤,\"\\
      harpoons.blbr: \"⥥,\"\\
      harpoons.bltr: \"⥯,\"\\
      harpoons.lbrb: \"⥧,\"\\
      harpoons.ltlb: \"⥢,\"\\
      harpoons.ltrb: \"⇋,\"\\
      harpoons.ltrt: \"⥦,\"\\
      harpoons.rblb: \"⥩,\"\\
      harpoons.rtlb: \"⇌,\"\\
      harpoons.rtlt: \"⥨,\"\\
      harpoons.tlbr: \"⥮,\"\\
      harpoons.tltr: \"⥣,\"\\
      tack.r: \"⊢,\"\\
      tack.r.not: \"⊬,\"\\
      tack.r.long: \"⟝,\"\\
      tack.r.short: \"⊦,\"\\
      tack.r.double: \"⊨,\"\\
      tack.r.double.not: \"⊭,\"\\
      tack.l: \"⊣,\"\\
      tack.l.long: \"⟞,\"\\
      tack.l.short: \"⫞,\"\\
      tack.l.double: \"⫤,\"\\
      tack.t: \"⊥,\"\\
      tack.t.big: \"⟘,\"\\
      tack.t.double: \"⫫,\"\\
      tack.t.short: \"⫠,\"\\
      tack.b: \"⊤,\"\\
      tack.b.big: \"⟙,\"\\
      tack.b.double: \"⫪,\"\\
      tack.b.short: \"⫟,\"\\
      tack.l.r: \"⟛,\"\\
      alpha: \"α,\"\\
      beta: \"β,\"\\
      beta.alt: \"ϐ,\"\\
      chi: \"χ,\"\\
      delta: \"δ,\"\\
      epsilon: \"ε,\"\\
      epsilon.alt: \"ϵ,\"\\
      eta: \"η,\"\\
      gamma: \"γ,\"\\
      iota: \"ι,\"\\
      kai: \"ϗ,\"\\
      kappa: \"κ,\"\\
      kappa.alt: \"ϰ,\"\\
      lambda: \"λ,\"\\
      mu: \"μ,\"\\
      nu: \"ν,\"\\
      ohm: \"Ω,\"\\
      ohm.inv: \"℧,\"\\
      omega: \"ω,\"\\
      omicron: \"ο,\"\\
      phi: \"φ,\"\\
      phi.alt: \"ϕ,\"\\
      pi: \"π,\"\\
      pi.alt: \"ϖ,\"\\
      psi: \"ψ,\"\\
      rho: \"ρ,\"\\
      rho.alt: \"ϱ,\"\\
      sigma: \"σ,\"\\
      sigma.alt: \"ς,\"\\
      tau: \"τ,\"\\
      theta: \"θ,\"\\
      theta.alt: \"ϑ,\"\\
      upsilon: \"υ,\"\\
      xi: \"ξ,\"\\
      zeta: \"ζ,\"\\
      Alpha: \"Α,\"\\
      Beta: \"Β,\"\\
      Chi: \"Χ,\"\\
      Delta: \"Δ,\"\\
      Epsilon: \"Ε,\"\\
      Eta: \"Η,\"\\
      Gamma: \"Γ,\"\\
      Iota: \"Ι,\"\\
      Kai: \"Ϗ,\"\\
      Kappa: \"Κ,\"\\
      Lambda: \"Λ,\"\\
      Mu: \"Μ,\"\\
      Nu: \"Ν,\"\\
      Omega: \"Ω,\"\\
      Omicron: \"Ο,\"\\
      Phi: \"Φ,\"\\
      Pi: \"Π,\"\\
      Psi: \"Ψ,\"\\
      Rho: \"Ρ,\"\\
      Sigma: \"Σ,\"\\
      Tau: \"Τ,\"\\
      Theta: \"Θ,\"\\
      Upsilon: \"Υ,\"\\
      Xi: \"Ξ,\"\\
      Zeta: \"Ζ,\"\\
      aleph: \"א,\"\\
      alef: \"א,\"\\
      beth: \"ב,\"\\
      bet: \"ב,\"\\
      gimmel: \"ג,\"\\
      gimel: \"ג,\"\\
      shin: \"ש,\"\\
      AA: \"𝔸,\"\\
      BB: \"𝔹,\"\\
      CC: \"ℂ,\"\\
      DD: \"𝔻,\"\\
      EE: \"𝔼,\"\\
      FF: \"𝔽,\"\\
      GG: \"𝔾,\"\\
      HH: \"ℍ,\"\\
      II: \"𝕀,\"\\
      JJ: \"𝕁,\"\\
      KK: \"𝕂,\"\\
      LL: \"𝕃,\"\\
      MM: \"𝕄,\"\\
      NN: \"ℕ,\"\\
      OO: \"𝕆,\"\\
      PP: \"ℙ,\"\\
      QQ: \"ℚ,\"\\
      RR: \"ℝ,\"\\
      SS: \"𝕊,\"\\
      TT: \"𝕋,\"\\
      UU: \"𝕌,\"\\
      VV: \"𝕍,\"\\
      WW: \"𝕎,\"\\
      XX: \"𝕏,\"\\
      YY: \"𝕐,\"\\
      ZZ: \"ℤ,\"\\
      ell: \"ℓ,\"\\
      planck: \"ℎ,\"\\
      planck.reduce: \"ℏ,\"\\
      angstrom: \"Å,\"\\
      kelvin: \"K,\"\\
      Re: \"ℜ,\"\\
      Im: \"ℑ,\"\\
      dotless.i: \"𝚤,\"\\
      dotless.j: \"𝚥\"
    $", -1, -1, 3, true, true, vec![], vec![]);

    println!("{:?}", parsed.decorations);
}
