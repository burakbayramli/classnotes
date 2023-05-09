# emacs-ipython

Emacs'te LaTeX modundan direk iPython kodu isletip sonucu metin /
grafik olarak alip yine LaTeX icinde gosterebilen Emacs eklentisi
emacs-ipython'u ayri bir proje olarak yayinladik. Indirin ve
icindeki ipython-md.py ve ipython-tex.py dosyalarini bir yere koyun.

Kurmak icin once bir sanal ortam yaratin, Python 2, 3 farketmez.

pip ile mock, ipython, matplotlib kurulmali.

Matplotlib icin ~/.config/matplotlib/matplotlibrc icinde

backend : TxAgg

ya da agg, ya da GTKAgg ya da TkAgg yazmak gerekebilir.

http://sayilarvekuramlar.blogspot.com/2018/08/virtualenv-python-izole-sanal-calsma.html

Ortama source ile girin. Simdi Pymacs alin

https://github.com/pinard/Pymacs

emacs-ipython indirin, acin,

Acip python setup.py install ile kurun

emacs.el icinde

```
(load-file "[PYMACS DIZIN]/pymacs.el")

(require 'tempo)


(autoload 'pymacs-apply "pymacs")

(autoload 'pymacs-call "pymacs")

(autoload 'pymacs-eval "pymacs" nil t)

(autoload 'pymacs-exec "pymacs" nil t)

(autoload 'pymacs-load "pymacs" nil t)





(defun reload-pymacs()

  (interactive)

  (if (buffer-live-p (get-buffer "*Pymacs*" ))

      (kill-buffer (get-buffer "*Pymacs*")))

  (message (buffer-file-name (current-buffer)))

  ;;

  ;; load tex or md mode based on the extension

  (if (equal (file-name-extension (buffer-file-name (current-buffer))) "tex")

      (progn 

 (pymacs-load "[DIZIN]/ipython-tex")

 (global-set-key "\M-," 'ipython-tex-run-py-code)

 (global-set-key [f5] 'ipython-tex-complete-py)

 (tempo-define-template 

  "tex-listings-python" 

  '("\\begin{minted}[fontsize=\\footnotesize]{python}\n"

    (s)

    "\n\\end{minted}\n"

    )

  "") 

 ))

  (if (equal (file-name-extension (buffer-file-name (current-buffer))) "md")

      (progn 

 (pymacs-load "[DIZIN]/ipython-md")

 (global-set-key "\M-," 'ipython-md-run-py-code)

 (global-set-key [f5] 'ipython-md-complete-py)

 (tempo-define-template 

  "tex-listings-python" 

  '("```python\n"

    (s)

    "\n```\n"

    )

  "") 

 ))

  )

```

Emacs'i source ile sanal ortama girdikten sonra baslatin cunku sadece
bu sekilde artik dogru python islemcisi bulunabilir.

Artik herhangi bir md, tex dosyasinda M-x reload-pymacs deyince
eklenti yuklenecektir. 








