%% Class file for the Proceedings of ECCT01
%% Class file for the Proceedings of ECCT99
%% Version 19.12.2000 T. Veijola
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{ecctd01}[1999/11/15 ECCTD 01 Proceedings Class]
\LoadClass[10pt,twocolumn,a4paper]{article}


%% Reset margins and text grid
\textwidth=\paperwidth
\advance\textwidth -2in
\columnwidth \textwidth \advance\columnwidth -\columnsep
\divide\columnwidth \tw@
\oddsidemargin \z@
\evensidemargin \z@
\advance \textheight \headheight \headheight \z@
\advance\textheight \headsep \headsep \z@
\advance \textheight \footskip \footskip \z@
\topmargin \paperheight \advance\topmargin -\textheight
\divide\topmargin \tw@ \advance\topmargin -1in
% Imitate appriximately the line spacing in MS-Word
%\renewcommand{\baselinestretch}{1.05}

%% Redefine \@maketitle

\def\@maketitle{%
  \newpage
  \null
%  \vskip 2em%
  {\centering
  \let \footnote \thanks
    {\LARGE\bfseries \@title \par}%
    \vskip 14pt%
    {\large
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
%    \vskip 1em%
%    {\large \@date}%
  \par}
  \vskip 14pt}

%% Section headings
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-2.5ex \@plus -1ex \@minus -.2ex}%
                                   {2ex \@plus.2ex}%
                                   {\normalfont\bfseries}}

\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-2.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\bfseries}}
%% Redefinition of \@makecaption
\long\def\@makecaption#1#2{\def\@capref{table}
  \ifx\@capref\@captype
   \vskip\belowcaptionskip
  \else
   \vskip\abovecaptionskip
  \fi
  \sbox\@tempboxa{#1: #2}%
  \ifdim \wd\@tempboxa >\hsize
    #1: #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \ifx\@capref\@captype
   \vskip\abovecaptionskip
  \else
   \vskip\belowcaptionskip
  \fi}

\renewenvironment{abstract}{%
    \footnotesize\bf%
    {\noindent\bf\textit{\abstractname{}} --- $\!$}}%
    {\mdseries\par}

%% Roman subscripts
\newcommand*{\ped}[1]{\ensuremath{_\mathrm{#1}}}

%% Reduce warning messages
\hbadness 5000 \vbadness 5000
\global\let\thepage\relax
\endinput


