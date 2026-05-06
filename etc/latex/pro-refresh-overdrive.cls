%%% pro-refresh-overdrive.cls --- Org export flagship class

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pro-refresh-overdrive}[2026/04/29 Flagship Org export class]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass{article}

\RequirePackage{geometry}
\geometry{top=12mm,bottom=12mm,left=16mm,right=16mm}
\RequirePackage{xcolor}
\RequirePackage{fontspec}
\RequirePackage{needspace}
\RequirePackage{etoolbox}
\RequirePackage{afterpage}
\RequirePackage{array}
\RequirePackage{booktabs}
\RequirePackage{tabularx}
\RequirePackage{colortbl}
\RequirePackage{tikz}
\usetikzlibrary{backgrounds}
\RequirePackage{pgfplots}
\pgfplotsset{compat=1.18}
\RequirePackage{graphicx}
\RequirePackage{fancyvrb}
\RequirePackage{tcolorbox}
\tcbuselibrary{raster,minted}

\tcbset{
  hubcard/.style={
    colframe=HubLine,
    boxrule=0.8pt,
    arc=0pt,
    outer arc=0pt,
    boxsep=0pt,
    left=12pt,
    right=12pt,
    top=12pt,
    bottom=12pt
  }
}

\definecolor{HubPaper}{HTML}{FCF5EE}
\definecolor{HubInk}{HTML}{0C3035}
\definecolor{HubAccent}{HTML}{FF550F}
\definecolor{HubMuted}{HTML}{543F21}
\definecolor{HubStandfirst}{HTML}{7A603D}
\definecolor{HubLabelBrown}{HTML}{7A3119}
\definecolor{HubLine}{HTML}{D8B5A4}
\definecolor{HubTableRule}{HTML}{FF9A52}
\definecolor{HubSurface}{HTML}{FFF4EC}
\definecolor{HubSurfaceSoft}{HTML}{FFFBF7}
\definecolor{HubMetricSurface}{HTML}{F3FBF7}
\definecolor{HubMint}{HTML}{20C997}
\definecolor{HubMintSoft}{HTML}{E6FAF4}

\AtBeginDocument{\color{HubInk}}
\AtBeginDocument{\pagecolor{HubPaper}}
\AtBeginDocument{\hypersetup{colorlinks=true,linkcolor=HubAccent,urlcolor=HubAccent}}
\defaultfontfeatures{Ligatures=TeX}
\setmainfont{Inter}
\setsansfont{Inter}
\setmonofont{Menlo}
\newfontface\HubDisplayFont{Inter ExtraBold}
\newfontface\HubQuoteFont{TeX Gyre Termes}

\newcolumntype{Y}{>{\raggedright\arraybackslash\hspace{0pt}}X}
\newcommand{\HubTableHeaderCell}[1]{{\color{HubLabelBrown}\bfseries\fontsize{8.5}{10}\selectfont\MakeUppercase{#1}\strut}}
\newcommand{\HubTableBodyCell}[1]{{\fontsize{8.4}{10.6}\selectfont\color{HubInk}#1\strut}}
\newcommand{\HubTableHeadRule}{\noalign{\vskip 1.8mm\color{HubTableRule}\hrule height 0.95pt\vskip 1.8mm}}
\newcommand{\HubTableBodyRule}{\noalign{\vskip 1.8mm\color{HubTableRule}\hrule height 0.55pt\vskip 1.8mm}}
\newenvironment{hubtable}{%
  \par\medskip\noindent
  \begingroup
  \setlength{\tabcolsep}{4.5pt}%
  \renewcommand{\arraystretch}{1.18}%
  \fontsize{8.4}{10.6}\selectfont\color{HubInk}%
}{%
  \endgroup
  \par\medskip
}

\renewcommand{\familydefault}{\sfdefault}
\makeatletter
\newcommand{\HubDocumentTitle}{}
\newlength{\HubFooterLift}
\setlength{\HubFooterLift}{4mm}
\newlength{\HubFooterBleed}
\setlength{\HubFooterBleed}{7mm}
\newlength{\HubFooterLogoGap}
\setlength{\HubFooterLogoGap}{3cm}
\newlength{\HubFooterRuleOffset}
\setlength{\HubFooterRuleOffset}{-1mm}
\newlength{\HubFooterContentOffset}
\setlength{\HubFooterContentOffset}{2mm}

\def\ps@hubpage{%
  \def\@oddhead{}%
  \def\@evenhead{}%
  \def\@oddfoot{%
    \ifnum\value{page}>1\relax
      \raisebox{\HubFooterLift}[0pt][0pt]{%
        \makebox[\textwidth][c]{%
          \parbox{\dimexpr\textwidth+2\HubFooterBleed\relax}{%
            {\color{HubLine}\raisebox{\HubFooterRuleOffset}[\height][\depth]{\rule{\linewidth}{0.5pt}}}\vspace{12pt}\par
            \raisebox{\HubFooterContentOffset}[\height][\depth]{%
              \parbox{\linewidth}{%
                \color{HubMuted}\footnotesize\sffamily
                \makebox[0pt][l]{%
                  \raisebox{-0.2\height}{\HubFooterLogoGraphic}\hspace{\HubFooterLogoGap}%
                }%
                \hfill
                \makebox[0pt][c]{\textit{\HubDocumentTitle}}%
                \hfill
                \makebox[0pt][r]{\thepage}%
              }%
            }%
          }%
        }%
      }%
    \fi
  }%
  \def\@evenfoot{\@oddfoot}%
}
\makeatother
\pagestyle{hubpage}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.6em}
\setlength{\tabcolsep}{5pt}
\renewcommand{\arraystretch}{1.2}
\arrayrulecolor{HubLine}
\renewcommand*\descriptionlabel[1]{\normalfont\color{HubLabelBrown}{\textbullet}\hspace{0.55em}\bfseries #1:\hspace{0.35em}}
\renewenvironment{description}{%
  \list{}{%
    \labelwidth\z@
    \leftmargin1.05em
    \itemindent-1.05em
    \labelsep\z@
    \itemsep0.45em
    \topsep0.65em
    \parsep\z@
    \listparindent\z@
    \let\makelabel\descriptionlabel
  }%
}{\endlist}

\makeatletter
\newcommand{\HubExportEyebrowBlock}{}
\newcommand{\HubHeroEyebrow}[1]{\vspace*{-0.8em}{\addfontfeature{Renderer=ICU,LetterSpace=16.0}\color{HubAccent}\small\HubDisplayFont\MakeUppercase{#1}\par}\vspace*{2em}}
\newcommand{\HubHeroTitle}[1]{%
  \gdef\HubDocumentTitle{#1}%
  {\HubDisplayFont\fontsize{34}{33}\selectfont #1\par}\vspace*{2.75em}%
}
\newcommand{\HubHeroDek}[1]{{\color{HubMuted}\parbox[t]{112mm}{\raggedright\normalsize #1\par}}\vspace*{4.5em}}
\newcommand{\HubHeroMeta}[2]{{\vspace*{0.75em}\color{HubLine}\rule{116mm}{0.8pt}\par}\vspace*{-0.3em}{\color{HubMuted}\fontsize{8}{9}\selectfont\makebox[116mm][r]{#1\HubMetaSeparator #2}\par}}
\providecommand{\HubHeroPatternGraphic}{\includegraphics[width=0.88\paperwidth]{hero-pattern.png}}
\providecommand{\HubHeroLogoGraphic}{\includegraphics[width=28mm]{hero-logo.pdf}}
\providecommand{\HubFooterLogoGraphic}{\includegraphics[width=16mm]{hero-logo.pdf}}
\newcommand{\HubMetaSeparator}{\hspace{0.8em}{\color{HubLine}\textbullet}\hspace{0.8em}}
\renewcommand{\thesection}{\ifnum\c@section<10 0\fi\arabic{section}}
\setcounter{secnumdepth}{1}
\def\@seccntformat#1{{\color{HubAccent}\csname the#1\endcsname}\hspace{0.75em}}
\newcommand{\HubSectionFormat}[1]{\parbox{\linewidth}{#1\par\vspace*{-0.6\baselineskip}{\color{HubLine}\rule{\linewidth}{1.5pt}}}}
\renewcommand\section{\Needspace{9\baselineskip}\@startsection{section}{1}{\z@}{2.8ex}{1.1ex}{\normalfont\Large\bfseries\raggedright\color{HubInk}\HubSectionFormat}}
\renewcommand\subsection{\Needspace{7\baselineskip}\@startsection{subsection}{2}{\z@}{2.2ex}{0.9ex}{\normalfont\large\bfseries\raggedright\color{HubMuted}}}
\renewcommand\subsubsection{\Needspace{6\baselineskip}\@startsection{subsubsection}{3}{\z@}{2.0ex}{0.8ex}{\normalfont\normalsize\bfseries\raggedright\color{HubMuted}}}
\makeatother

\newlength{\HubHeroTopInset}
\setlength{\HubHeroTopInset}{3mm}
\newlength{\HubHeroSideInset}
\setlength{\HubHeroSideInset}{16mm}

\newcommand{\HubQuoteOpenMark}{{\color{HubMuted}\bfseries``}}
\newcommand{\HubQuoteCloseMark}{{\color{HubMuted}\bfseries''}}
\newcommand{\HubDecorativeQuoteMark}{{\HubQuoteFont\fontsize{112}{112}\selectfont\rlap{”}\hspace{0.35pt}\rlap{”}\hspace{0.35pt}”}}
\newcommand{\HubQuoteFrame}[1]{%
  \begin{tikzpicture}
    \node[draw=HubLine,line width=0.8pt,fill=HubSurface,inner xsep=32pt,inner ysep=29pt,outer sep=0pt,
          path picture={\node[anchor=south east,inner sep=0pt,text=HubAccent!14!HubLine,text opacity=0.18] at ([xshift=-8pt,yshift=-10pt]path picture bounding box.south east) {\HubDecorativeQuoteMark};}] (hubquote) {#1};
    \fill[HubAccent] ([xshift=-0.4pt,yshift=0.4pt]hubquote.north west) rectangle ([xshift=8.4pt,yshift=-0.4pt]hubquote.south west);
  \end{tikzpicture}%
}

\newenvironment{hubhero}{%
  \par\noindent
  \begin{tikzpicture}[overlay]
    \fill[HubPaper] (-\HubHeroSideInset, \HubHeroTopInset + \topskip) rectangle (\paperwidth - \HubHeroSideInset, \HubHeroTopInset + \topskip - \paperheight);
    \coordinate (CardNW) at (\dimexpr -\HubHeroSideInset + 5mm\relax, \dimexpr \HubHeroTopInset + \topskip + 1cm\relax);
    \coordinate (CardNE) at (\textwidth + \HubHeroSideInset, \HubHeroTopInset + \topskip);
    \node[anchor=north west, opacity=0.18, inner sep=0pt] at (CardNW) {\HubHeroPatternGraphic};
    \node[anchor=north east, inner sep=0pt] at (CardNE) [xshift=-\HubHeroSideInset, yshift=-\HubHeroTopInset] {\HubHeroLogoGraphic};
  \end{tikzpicture}%
  \par\noindent
}{%
  \par\vspace{35mm}
}
\newsavebox{\HubQuoteBox}
\renewenvironment{quote}[1][]{%
  \def\HubQuoteAttribution{#1}%
  \par\medskip\noindent
  \begin{lrbox}{\HubQuoteBox}%
  \begin{minipage}{\dimexpr\linewidth-48pt\relax}%
    \setlength{\parskip}{0.6em}%
    \fontsize{12.5}{16}\selectfont\bfseries\color{HubInk}\ignorespaces
    \HubQuoteOpenMark
}{%
    \HubQuoteCloseMark
    \ifx\HubQuoteAttribution\empty\else
      \par\vspace{45.14pt}{\raggedleft\normalfont\small\color{HubMuted}\HubQuoteAttribution\par}\vspace{-14pt}%
    \fi
  \end{minipage}%
  \end{lrbox}%
  \HubQuoteFrame{\usebox{\HubQuoteBox}}%
  \par\medskip
}
\newenvironment{standfirst}{\par\smallskip\noindent\color{HubStandfirst}\large\bfseries\ignorespaces}{\par\medskip}
\newenvironment{pullquote}[1][]{%
  \par\medskip\noindent
  \begingroup
  \raggedright\fontsize{15.5}{21}\selectfont\color{HubInk}\ignorespaces
}{%
  \par
  \endgroup
  \par\medskip
}
\newenvironment{callout}[1][]{%
  \par\medskip\noindent
  \begin{tcolorbox}[hubcard, colback=HubSurface]
    \def\HubTempArg{#1}%
    \ifx\HubTempArg\empty\else{\raggedright\textbf{#1}\par}\vspace{1em}\fi
    \small\color{HubInk}\ignorespaces
}{%
  \end{tcolorbox}%
  \par\medskip
}
\makeatletter
\newenvironment{metrics}{%
  \par\medskip\noindent%
  \begin{tcbraster}[raster columns=4, raster equal height=rows, raster column skip=0.02\linewidth]%
  \let\hub@oldpar\par
  \let\par\relax
}{%
  \let\par\hub@oldpar
  \end{tcbraster}%
  \par\medskip
}
\newenvironment{metric}[1][]{%
  \let\par\hub@oldpar
  \begin{tcolorbox}[
    colback=HubMetricSurface,
    colframe=HubLine,
    boxrule=0.8pt,
    arc=0pt,
    outer arc=0pt,
    boxsep=0pt,
    left=15pt,
    right=15pt,
    top=12pt,
    bottom=12pt
  ]
    {\centering\textcolor{HubAccent}{\bfseries\Large #1}\par}
    \vspace{1.2em}%
    \raggedright\small\color{HubMuted}\ignorespaces
}{%
  \end{tcolorbox}%
  \let\par\relax
}
\makeatother
\makeatletter
\newcounter{hubpillarenv}
\newcounter{hubpillaritem}
\newenvironment{pillars}{%
  \stepcounter{hubpillarenv}%
  \par\medskip\noindent%
  \ifcsname hub@pillar@count@\thehubpillarenv\endcsname
    \ifnum\csname hub@pillar@count@\thehubpillarenv\endcsname>4
      \def\hub@cols{4}%
    \else
      \edef\hub@cols{\csname hub@pillar@count@\thehubpillarenv\endcsname}%
    \fi
  \else
    \def\hub@cols{4}% fallback
  \fi
  \setcounter{hubpillaritem}{0}%
  \begin{tcbraster}[raster columns=\hub@cols, raster equal height=rows, raster column skip=0.02\linewidth]%
  \let\hub@oldpar\par
  \let\par\relax
}{%
  \let\par\hub@oldpar
  \end{tcbraster}%
  \immediate\write\@auxout{\string\expandafter\string\gdef\string\csname\space hub@pillar@count@\thehubpillarenv\string\endcsname{\thehubpillaritem}}%
  \par\medskip
}
\newenvironment{pillar}[1][]{%
  \stepcounter{hubpillaritem}%
  \let\par\hub@oldpar
  \begin{tcolorbox}[hubcard, colback=white]
    \def\HubTempArg{#1}%
    \ifx\HubTempArg\empty\else{\raggedright\textbf{#1}\par}\vspace{1em}\fi
    \small\color{HubInk}\ignorespaces
}{%
  \end{tcolorbox}%
  \let\par\relax
}
\makeatother
\colorlet{HubCodeLineNumberLight}{HubMuted!60!HubSurfaceSoft}
\colorlet{HubCodeLineNumberDark}{HubStandfirst!50!HubSurfaceSoft}

\newcommand{\HubCodeThemeLight}{%
  \tcbset{
    hubcode/.style={
      colback=HubSurfaceSoft,
      colframe=HubLine,
      boxrule=0.5pt,
      arc=4pt,
      left=4pt, right=4pt, top=4pt, bottom=4pt,
      fontupper=\color{HubInk},
    }
  }%
  \def\HubCodeLineNumColor{HubCodeLineNumberLight}%
  \def\HubCodeKeywordColor{HubAccent}%
  \def\HubCodeStringColor{HubTableRule}%
  \def\HubCodeCommentColor{HubMuted}%
  \def\HubCodeNameColor{HubStandfirst}%
  \def\HubCodeMintColor{HubMint}%
  \def\HubCodeTextColor{HubInk}%
  \def\HubCodeOperatorColor{HubMuted}%
}

\newcommand{\HubCodeThemeDark}{%
  \tcbset{
    hubcode/.style={
      colback=HubInk,
      colframe=HubStandfirst,
      boxrule=0.5pt,
      arc=4pt,
      left=4pt, right=4pt, top=4pt, bottom=4pt,
      fontupper=\color{HubSurfaceSoft},
    }
  }%
  \def\HubCodeLineNumColor{HubCodeLineNumberDark}%
  \def\HubCodeKeywordColor{HubAccent}%
  \def\HubCodeStringColor{HubTableRule}%
  \def\HubCodeCommentColor{HubMuted}%
  \def\HubCodeNameColor{HubSurfaceSoft}%
  \def\HubCodeMintColor{HubMint}%
  \def\HubCodeTextColor{HubSurfaceSoft}%
  \def\HubCodeOperatorColor{HubStandfirst}%
}

\HubCodeThemeLight

\makeatletter
\AtBeginEnvironment{MintedVerbatim}{%
  \renewcommand{\theFancyVerbLine}{\textcolor{\HubCodeLineNumColor}{\ttfamily\arabic{FancyVerbLine}}}%
  \@namedef{PYG@tok@k}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@kc}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@kd}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@kn}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@kp}{\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@kr}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@kt}{\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@ow}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeKeywordColor}{##1}}}%
  \@namedef{PYG@tok@s}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@sa}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@sb}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@sc}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@dl}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@sd}{\let\PYG@it=\textit\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@s2}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@se}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@sh}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@si}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@sx}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@sr}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@s1}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@ss}{\def\PYG@tc##1{\textcolor{\HubCodeStringColor}{##1}}}%
  \@namedef{PYG@tok@c}{\let\PYG@it=\textit\def\PYG@tc##1{\textcolor{\HubCodeCommentColor}{##1}}}%
  \@namedef{PYG@tok@ch}{\let\PYG@it=\textit\def\PYG@tc##1{\textcolor{\HubCodeCommentColor}{##1}}}%
  \@namedef{PYG@tok@cm}{\let\PYG@it=\textit\def\PYG@tc##1{\textcolor{\HubCodeCommentColor}{##1}}}%
  \@namedef{PYG@tok@cpf}{\let\PYG@it=\textit\def\PYG@tc##1{\textcolor{\HubCodeCommentColor}{##1}}}%
  \@namedef{PYG@tok@c1}{\let\PYG@it=\textit\def\PYG@tc##1{\textcolor{\HubCodeCommentColor}{##1}}}%
  \@namedef{PYG@tok@cs}{\let\PYG@it=\textit\def\PYG@tc##1{\textcolor{\HubCodeCommentColor}{##1}}}%
  \@namedef{PYG@tok@nb}{\def\PYG@tc##1{\textcolor{\HubCodeMintColor}{##1}}}%
  \@namedef{PYG@tok@bp}{\def\PYG@tc##1{\textcolor{\HubCodeMintColor}{##1}}}%
  \@namedef{PYG@tok@fm}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@vc}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@vg}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@vi}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@vm}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@no}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@nl}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@na}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@n}{\def\PYG@tc##1{\textcolor{\HubCodeTextColor}{##1}}}%
  \@namedef{PYG@tok@nf}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@nc}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeMintColor}{##1}}}%
  \@namedef{PYG@tok@nd}{\def\PYG@tc##1{\textcolor{\HubCodeMintColor}{##1}}}%
  \@namedef{PYG@tok@ne}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@ni}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@nn}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@nt}{\let\PYG@bf=\textbf\def\PYG@tc##1{\textcolor{\HubCodeMintColor}{##1}}}%
  \@namedef{PYG@tok@nv}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@o}{\def\PYG@tc##1{\textcolor{\HubCodeOperatorColor}{##1}}}%
  \@namedef{PYG@tok@m}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@mb}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@mf}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@mh}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@mi}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@il}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
  \@namedef{PYG@tok@mo}{\def\PYG@tc##1{\textcolor{\HubCodeNameColor}{##1}}}%
}
\makeatother
\BeforeBeginEnvironment{minted}{\begin{tcolorbox}[hubcode]}
\AfterEndEnvironment{minted}{\end{tcolorbox}}

\pgfplotsset{
  hubgraphaxis/.style={
    axis lines=left,
    axis line style={HubLine, thick},
    tick style={HubLine, thick},
    grid style={HubLine!50, dashed},
    label style={font=\sffamily\small\color{HubMuted}},
    tick label style={font=\sffamily\footnotesize\color{HubMuted}, /pgf/number format/assume math mode=true},
    legend style={
      draw=none,
      fill=none,
      font=\sffamily\footnotesize\color{HubInk},
      cells={anchor=west},
      at={(0.98,0.05)},
      anchor=south east
    },
    every axis plot/.append style={very thick},
    cycle list={
      {HubAccent, solid},
      {HubStandfirst, dashed},
      {HubLabelBrown, dotted}
    }
  }
}

\newenvironment{graph}[1][]{%
  \par\medskip\noindent
  \begingroup
    \def\HubTempArg{#1}%
    \ifx\HubTempArg\empty\else{\raggedright\textbf{#1}\par}\vspace{1em}\fi
    \centering
}{%
  \par
  \endgroup
  \par\medskip
}

\newenvironment{hubfootnote}{\par\medskip\footnotesize\color{HubMuted}}{\par}

\endinput
