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
