%%% hub-article.cls --- Personal Org article class

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hub-article}[2026/05/10 Personal Org article class]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass{article}

\RequirePackage{iftex}
\ifXeTeX
  \RequirePackage{fontspec}
  \defaultfontfeatures{Ligatures=TeX}
\else
  \ClassError{hub-article}{hub-article requires XeLaTeX}{Compile this class with XeLaTeX.}
\fi

\RequirePackage{xcolor}
\RequirePackage{microtype}

\definecolor{HubArticleInk}{HTML}{18312D}
\definecolor{HubArticleMuted}{HTML}{64756F}
\definecolor{HubArticleAccentMuted}{HTML}{3D7A70}
\definecolor{HubArticleRule}{HTML}{CBD8D2}
\definecolor{HubArticleStandfirst}{HTML}{2E514B}

\setmainfont{TeX Gyre Pagella}[
  Ligatures=TeX,
  Numbers=OldStyle]
\setsansfont{TeX Gyre Heros}[
  Ligatures=TeX,
  Scale=MatchLowercase]
\setmonofont{Latin Modern Mono}[
  Scale=MatchLowercase]
\newfontfamily\HubArticleSerif{TeX Gyre Pagella}[
  Ligatures=TeX,
  Numbers=OldStyle]
\newfontfamily\HubArticleSans{TeX Gyre Heros}[
  Ligatures=TeX,
  Scale=MatchLowercase]
\newfontfamily\HubArticleMono{Latin Modern Mono}[
  Scale=MatchLowercase]

\AtBeginDocument{\color{HubArticleInk}}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.62em}

\newcommand{\HubArticleDocumentTitle}{}
\newcommand{\HubArticleEyebrowBlock}{}
\newcommand{\HubArticleMetaSeparator}{\hspace{0.65em}{\textcolor{HubArticleAccentMuted}{\textbullet}}\hspace{0.65em}}
\newcommand{\HubArticleAccentMarker}{\textcolor{HubArticleAccentMuted}{\rule{26mm}{0.7pt}}}
\newcommand{\HubArticleEyebrow}[1]{%
  {\HubArticleSans\small\addfontfeature{LetterSpace=10.0}\textcolor{HubArticleAccentMuted}{\MakeUppercase{#1}}\par}%
  \vspace{1.15em}%
}
\newcommand{\HubArticleTitle}[1]{%
  \gdef\HubArticleDocumentTitle{#1}%
  {\HubArticleSerif\fontsize{28}{32}\selectfont\textcolor{HubArticleInk}{#1}\par}%
  \vspace{0.8em}%
}
\newcommand{\HubArticleDek}[1]{%
  \def\HubArticleTemp{#1}%
  \ifx\HubArticleTemp\empty\else
    {\HubArticleSerif\large\itshape\textcolor{HubArticleStandfirst}{#1}\par}%
    \vspace{1.2em}%
  \fi
}
\newcommand{\HubArticleMeta}[2]{%
  {\HubArticleSans\footnotesize\textcolor{HubArticleMuted}{#1\HubArticleMetaSeparator #2}\par}%
}

\newenvironment{hubarticleopener}{%
  \par\vspace*{1.1em}\noindent
  \begingroup
  \HubArticleAccentMarker\par
  \vspace{1.35em}%
}{%
  \par\vspace{1.8em}%
  \endgroup
}

\newenvironment{standfirst}{%
  \par\medskip\noindent
  \begingroup
  \HubArticleSerif\large\itshape\color{HubArticleStandfirst}\ignorespaces
}{%
  \par
  \endgroup
  \medskip
}
