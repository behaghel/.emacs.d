%%% veriff.cls --- Org export flagship class

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{veriff}[2026/04/29 Flagship Org export class]

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
\RequirePackage{caption}
\RequirePackage{fancyvrb}
\RequirePackage{tcolorbox}
\tcbuselibrary{raster,minted}
\RequirePackage[normalem]{ulem}

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

\tcbset{
  hubfigureimage/.style={
    colback=HubSurfaceSoft,
    colframe=HubLine,
    boxrule=0.8pt,
    arc=3pt,
    boxsep=0pt,
    left=6pt,
    right=6pt,
    top=6pt,
    bottom=6pt,
    center upper
  }
}

\definecolor{HubPaper}{HTML}{FBEDE8}
\definecolor{HubInk}{HTML}{0C3035}
\definecolor{HubAccent}{HTML}{FF550F}
\definecolor{HubMuted}{HTML}{543F21}
\definecolor{HubStandfirst}{HTML}{845D33}
\definecolor{HubLabelBrown}{HTML}{7D1F1F}
\definecolor{HubLine}{HTML}{C8B59A}
\definecolor{HubTableRule}{HTML}{FF9A52}
\definecolor{HubSurface}{HTML}{FBEDE8}
\definecolor{HubSurfaceSoft}{HTML}{F6FDFC}
\definecolor{HubMetricSurface}{HTML}{F6FDFC}
\definecolor{HubMint}{HTML}{14E5C5}
\definecolor{HubPatternMintVivid}{HTML}{9DF5EA}
\definecolor{HubMintSoft}{HTML}{E0FCF8}
\definecolor{HubTeal}{HTML}{14E5C5}
\definecolor{HubLightTeal}{HTML}{9DF5EA}
\definecolor{HubDekWarm}{HTML}{7D1F1F}
\definecolor{HubTeal500}{HTML}{14E5C5}
\definecolor{HubTeal700}{HTML}{085559}
\definecolor{HubTeal900}{HTML}{0C3035}
\definecolor{HubGalleryPaper}{HTML}{F6FDFC}
\definecolor{HubGallerySurface}{HTML}{F6FDFC}
\definecolor{HubGallerySurfaceSoft}{HTML}{E0FCF8}
\definecolor{HubGalleryLine}{HTML}{CBD7D4}
\definecolor{HubGalleryMuted}{HTML}{4B5F5A}

\colorlet{HubEyebrowColor}{HubAccent}
\colorlet{HubSectionNumColor}{HubAccent}
\colorlet{HubGraphPrimaryColor}{HubAccent}
\colorlet{HubDekColor}{HubMuted}
\colorlet{HubTableHeaderColor}{HubLabelBrown}
\colorlet{HubTermColor}{HubLabelBrown}
\colorlet{HubMetricValueColor}{HubAccent}
\colorlet{HubCardTitleColor}{HubInk}
\colorlet{HubStringColor}{HubTableRule}
\colorlet{HubSubsectionColor}{HubMuted}
\colorlet{HubTitleColor}{HubInk}
\colorlet{HubSectionTitleColor}{HubInk}
\colorlet{HubQuoteTextColor}{HubInk}
\colorlet{HubCalloutTextColor}{HubInk}
\colorlet{HubFigureCaptionTextColor}{HubStandfirst}
\colorlet{HubEmphasisColor}{HubAccent}
\colorlet{HubCalloutEdgeColor}{HubDekWarm}
\colorlet{HubCalloutTitleColor}{HubCalloutEdgeColor}
\colorlet{HubLinkColor}{HubAccent}
\colorlet{HubUrlColor}{HubAccent}
\colorlet{HubListMarkerColor}{HubAccent}
\colorlet{HubFootnoteTextColor}{HubMuted}
\colorlet{HubFootnoteMarkerColor}{HubAccent}

\definecolor{HubInlineCodeBrown}{HTML}{543F21}
\definecolor{HubInlineCodeBrownLifted}{HTML}{845D33}
\definecolor{HubInlineCodeOrange}{HTML}{FF9A52}
\definecolor{HubInlineCodeOrangeSoft}{HTML}{FBEDE8}
\colorlet{HubInlineCodeBg}{HubInlineCodeOrangeSoft}
\colorlet{HubInlineCodeText}{HubInlineCodeBrownLifted}
\colorlet{HubInlineCodeFrame}{HubAccent}

\newcommand{\HubInlineBold}[1]{{\color{HubEmphasisColor}\textbf{#1}}}
\newcommand{\HubInlineItalic}[1]{\textit{#1}}
\newcommand{\HubInlineUnderline}[1]{\uline{#1}}
\newcommand{\HubInlineStrike}[1]{\sout{#1}}

\newtcbox{\HubInlineCode}{on line,
  colback=HubInlineCodeBg,
  colframe=HubInlineCodeFrame,
  coltext=HubInlineCodeText,
  boxrule=0.5pt,
  arc=2pt,
  boxsep=0pt,
  left=2pt, right=2pt, top=1pt, bottom=1pt,
  fontupper=\ttfamily\bfseries\fontsize{7.5pt}{9pt}\selectfont
}

\newif\ifhubgallerywhite
\newcommand{\HubGalleryWhiteBodyFont}{\fontsize{8.9}{11.2}\selectfont}
\AtBeginDocument{\ifhubgallerywhite\HubGalleryWhiteBodyFont\fi}

\newcommand{\HubVeriffVariant}[1]{%
  \def\hub@variant{#1}%
  \ifdefstring{\hub@variant}{gallery-white}{%
    \hubgallerywhitetrue%
    \geometry{top=14mm,bottom=16mm,left=13mm,right=13mm,columnsep=7mm}%
    \colorlet{HubPaper}{HubGalleryPaper}%
    \colorlet{HubInk}{HubTeal900}%
    \colorlet{HubAccent}{HubTeal500}%
    \colorlet{HubMuted}{HubGalleryMuted}%
    \colorlet{HubStandfirst}{HubGalleryMuted}%
    \colorlet{HubLabelBrown}{HubTeal700}%
    \colorlet{HubLine}{HubGalleryLine}%
    \colorlet{HubTableRule}{HubGalleryLine}%
    \colorlet{HubSurface}{HubGallerySurface}%
    \colorlet{HubSurfaceSoft}{HubGallerySurfaceSoft}%
    \colorlet{HubMetricSurface}{HubGallerySurface}%
    \colorlet{HubEyebrowColor}{HubTeal700}%
    \colorlet{HubSectionNumColor}{HubGalleryMuted}%
    \colorlet{HubGraphPrimaryColor}{HubTeal700}%
    \colorlet{HubDekColor}{HubGalleryMuted}%
    \colorlet{HubTableHeaderColor}{HubTeal700}%
    \colorlet{HubTermColor}{HubTeal700}%
    \colorlet{HubMetricValueColor}{HubTeal900}%
    \colorlet{HubCardTitleColor}{HubTeal900}%
    \colorlet{HubStringColor}{HubTeal700}%
    \colorlet{HubSubsectionColor}{HubTeal900}%
    \colorlet{HubTitleColor}{HubTeal900}%
    \colorlet{HubSectionTitleColor}{HubTeal900}%
    \colorlet{HubQuoteTextColor}{HubTeal900}%
    \colorlet{HubCalloutTextColor}{HubTeal900}%
    \colorlet{HubFigureCaptionTextColor}{HubGalleryMuted}%
    \colorlet{HubEmphasisColor}{HubTeal700}%
    \colorlet{HubCalloutEdgeColor}{HubTeal700}%
    \colorlet{HubCalloutTitleColor}{HubCalloutEdgeColor}%
    \colorlet{HubLinkColor}{HubTeal700}%
    \colorlet{HubUrlColor}{HubTeal700}%
    \colorlet{HubListMarkerColor}{HubTeal700}%
    \colorlet{HubFootnoteTextColor}{HubGalleryMuted}%
    \colorlet{HubFootnoteMarkerColor}{HubTeal700}%
  }{}%
  \ifdefstring{\hub@variant}{dark-campaign}{%
    \definecolor{HubPaper}{HTML}{0C3035}%
    \definecolor{HubPaleBlue}{HTML}{F6FDFC}%
    \colorlet{HubInk}{HubPaleBlue}%
    \definecolor{HubAccent}{HTML}{FF550F}%
    \definecolor{HubMuted}{HTML}{97A19E}%
    \colorlet{HubStandfirst}{HubPaleBlue}%
    \definecolor{HubLabelBrown}{HTML}{FF9A52}%
    \definecolor{HubLine}{HTML}{4B5F5A}%
    \definecolor{HubTableRule}{HTML}{4B5F5A}%
    \definecolor{HubSurface}{HTML}{0C3035}%
    \definecolor{HubSurfaceSoft}{HTML}{0C3035}%
    \definecolor{HubMetricSurface}{HTML}{085559}%
    \definecolor{HubMint}{HTML}{14E5C5}%
    \definecolor{HubMintSoft}{HTML}{0C3035}%
    \definecolor{HubWhite}{HTML}{F6FDFC}%
    \colorlet{HubEyebrowColor}{HubTeal}%
    \colorlet{HubSectionNumColor}{HubTeal}%
    \colorlet{HubGraphPrimaryColor}{HubTeal}%
    \colorlet{HubDekColor}{HubLightTeal}%
    \colorlet{HubTableHeaderColor}{HubLightTeal}%
    \colorlet{HubTermColor}{HubLightTeal}%
    \colorlet{HubMetricValueColor}{HubWhite}%
    \colorlet{HubCardTitleColor}{HubWhite}%
    \colorlet{HubStringColor}{HubLightTeal}%
    \colorlet{HubSubsectionColor}{HubWhite}%
    \colorlet{HubTitleColor}{HubWhite}%
    \colorlet{HubSectionTitleColor}{HubWhite}%
    \colorlet{HubQuoteTextColor}{HubWhite}%
    \colorlet{HubCalloutTextColor}{HubWhite}%
    \colorlet{HubFigureCaptionTextColor}{HubMuted}%
    \colorlet{HubEmphasisColor}{HubTeal}%
    \colorlet{HubCalloutEdgeColor}{HubPatternMintVivid}%
    \colorlet{HubCalloutTitleColor}{HubCalloutEdgeColor}%
    \colorlet{HubLinkColor}{HubTeal}%
    \colorlet{HubUrlColor}{HubTeal}%
    \colorlet{HubListMarkerColor}{HubTeal}%
    \colorlet{HubFootnoteTextColor}{HubMuted}%
    \colorlet{HubFootnoteMarkerColor}{HubAccent}%
    \colorlet{HubInlineCodeBg}{HubInlineCodeBrown}%
    \colorlet{HubInlineCodeText}{HubInlineCodeOrange}%
    \colorlet{HubInlineCodeFrame}{HubAccent}%
  }{}%
}

\AtBeginDocument{\color{HubInk}}
\AtBeginDocument{\pagecolor{HubPaper}}
\AtBeginDocument{\hypersetup{colorlinks=true,linkcolor=HubLinkColor,urlcolor=HubUrlColor,citecolor=HubLinkColor}}
\defaultfontfeatures{Ligatures=TeX}
\setmainfont{Inter}
\setsansfont{Inter}
\setmonofont{Menlo}
\newfontface\HubDisplayFont{Inter ExtraBold}
\newfontface\HubQuoteFont{TeX Gyre Termes}

\newcolumntype{Y}{>{\raggedright\arraybackslash\hspace{0pt}}X}
\newcommand{\HubTableHeaderCell}[1]{{\color{HubTableHeaderColor}\bfseries\fontsize{8.5}{10}\selectfont\MakeUppercase{#1}\strut}}
\newcommand{\HubTableBodyCell}[1]{{\fontsize{8.4}{10.6}\selectfont\color{HubInk}#1\strut}}
\newcommand{\HubTableHeadRule}{\noalign{\vskip 1.8mm\color{HubTableRule}\hrule height 0.95pt\vskip 1.8mm}}
\newcommand{\HubTableBodyRule}{\noalign{\vskip 1.8mm\color{HubTableRule}\hrule height 0.55pt\vskip 1.8mm}}
\newcommand{\HubListBullet}{{\textcolor{HubListMarkerColor}{\raisebox{-0.3mm}{\scalebox{1.5}{\textbullet}}}}}
\newcommand{\HubListNumber}[1]{{\textcolor{HubListMarkerColor}{#1.}}}
\newcommand{\HubCheckboxBase}{%
  \tikz[baseline=0.16em]{\draw[HubListMarkerColor,line width=0.45pt,rounded corners=0.55pt] (0,0) rectangle (0.95em,0.95em);}%
}
\newcommand{\HubCheckboxChecked}{%
  \tikz[baseline=0.16em]{%
    \draw[HubListMarkerColor,line width=0.45pt,rounded corners=0.55pt] (0,0) rectangle (0.95em,0.95em);
    \draw[HubListMarkerColor,line width=0.8pt,line cap=round,line join=round] (0.2em,0.48em) -- (0.4em,0.25em) -- (0.75em,0.72em);}%
}
\newcommand{\HubCheckboxUnchecked}{\HubCheckboxBase}
\newcommand{\HubCheckboxPartial}{%
  \tikz[baseline=0.16em]{%
    \draw[HubListMarkerColor,line width=0.45pt,rounded corners=0.55pt] (0,0) rectangle (0.95em,0.95em);
    \draw[HubListMarkerColor,line width=0.8pt,line cap=round] (0.22em,0.48em) -- (0.73em,0.48em);}%
}
\newcommand{\HubFootnoteText}[1]{{\color{HubFootnoteTextColor}\footnotesize #1}}
\newcommand{\HubFigureImage}[1]{\begin{tcolorbox}[hubfigureimage]#1\end{tcolorbox}}
\DeclareCaptionFont{hubfigurecaption}{\fontsize{8.5}{10.2}\selectfont}
\captionsetup[figure]{font=hubfigurecaption,labelfont={color=HubAccent},textfont={it,color=HubFigureCaptionTextColor},labelsep=period,justification=centering,singlelinecheck=false,skip=4.5pt}
\renewcommand{\labelitemi}{\HubListBullet}
\renewcommand{\labelenumi}{\HubListNumber{\arabic{enumi}}}
\AtBeginEnvironment{itemize}{\setlength{\itemsep}{0.25em}\setlength{\topsep}{0.45em}\setlength{\parsep}{0pt}}
\AtBeginEnvironment{enumerate}{\setlength{\itemsep}{0.25em}\setlength{\topsep}{0.45em}\setlength{\parsep}{0pt}}
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
\renewcommand{\@makefnmark}{\hbox{\@textsuperscript{\normalfont\textcolor{HubFootnoteMarkerColor}{\@thefnmark}}}}
\newcommand{\HubDocumentTitle}{}
\newlength{\HubFooterLift}
\setlength{\HubFooterLift}{-0.5mm}
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
\renewcommand*\descriptionlabel[1]{\normalfont\HubListBullet\hspace{0.55em}{\color{HubTermColor}\bfseries #1:}\hspace{0.35em}}
\renewenvironment{description}{%
  \list{}{%
    \labelwidth0pt
    \leftmargin1.05em
    \itemindent-1.05em
    \labelsep0pt
    \itemsep0.45em
    \topsep0.65em
    \parsep0pt
    \listparindent0pt
    \let\makelabel\descriptionlabel
  }%
}{\endlist}

\makeatletter
\newcommand{\HubExportEyebrowBlock}{}
\newcommand{\HubHeroEyebrow}[1]{\vspace*{-0.8em}{\addfontfeature{Renderer=ICU,LetterSpace=16.0}\color{HubEyebrowColor}\small\HubDisplayFont\MakeUppercase{#1}\par}\vspace*{2em}}
\newcommand{\HubHeroTitle}[1]{%
  \gdef\HubDocumentTitle{#1}%
  {\color{HubTitleColor}\HubDisplayFont\fontsize{34}{33}\selectfont #1\par}\vspace*{2.75em}%
}
\newcommand{\HubHeroDek}[1]{%
  \ifhubgallerywhite
    {\color{HubDekColor}\parbox[t]{134mm}{\raggedright\fontsize{15}{18}\selectfont #1\par}}\vspace*{2.6em}%
  \else
    {\color{HubDekColor}\parbox[t]{112mm}{\raggedright\normalsize #1\par}}\vspace*{4.5em}%
  \fi
}
\newcommand{\HubHeroMeta}[2]{{\vspace*{0.75em}\color{HubLine}\rule{116mm}{0.8pt}\par}\vspace*{-0.3em}{\color{HubMuted}\fontsize{8}{9}\selectfont\makebox[116mm][r]{#1\HubMetaSeparator #2}\par}}
\providecommand{\HubHeroPatternGraphic}{\includegraphics[width=0.88\paperwidth]{hero-pattern.png}}
\providecommand{\HubHeroLogoGraphic}{\includegraphics[width=28mm]{hero-logo.pdf}}
\providecommand{\HubFooterLogoGraphic}{\includegraphics[width=16mm]{hero-logo.pdf}}
\newcommand{\HubMetaSeparator}{\hspace{0.8em}{\color{HubLine}\textbullet}\hspace{0.8em}}
\renewcommand{\thesection}{\ifnum\c@section<10 0\fi\arabic{section}}
\setcounter{secnumdepth}{1}
\def\@seccntformat#1{{\color{HubSectionNumColor}\csname the#1\endcsname}\hspace{0.75em}}
\newcommand{\HubSectionFormat}[1]{\parbox{\linewidth}{#1\par\vspace*{-0.6\baselineskip}{\color{HubLine}\rule{\linewidth}{1.5pt}}}}
\renewcommand\section{\Needspace{9\baselineskip}\@startsection{section}{1}{\z@}{2.8ex}{1.1ex}{\normalfont\Large\bfseries\raggedright\color{HubSectionTitleColor}\HubSectionFormat}}
\renewcommand\subsection{\Needspace{7\baselineskip}\@startsection{subsection}{2}{\z@}{2.2ex}{0.9ex}{\normalfont\large\bfseries\raggedright\color{HubSubsectionColor}}}
\renewcommand\subsubsection{\Needspace{6\baselineskip}\@startsection{subsubsection}{3}{\z@}{2.0ex}{0.8ex}{\normalfont\normalsize\bfseries\raggedright\color{HubSubsectionColor}}}
\makeatother

\newlength{\HubHeroTopInset}
\setlength{\HubHeroTopInset}{3mm}
\newlength{\HubHeroSideInset}
\setlength{\HubHeroSideInset}{16mm}

\newcommand{\HubQuoteOpenMark}{{\color{HubMuted}\bfseries``}}
\newcommand{\HubQuoteCloseMark}{{\color{HubMuted}\bfseries''}}
\newsavebox{\HubEpigraphBox}
\newcommand{\HubEpigraphFrame}[1]{%
  \begin{flushright}%
    \begin{tikzpicture}
      \node[inner xsep=18pt,inner ysep=14pt,outer sep=0pt] (hubepigraph) {#1};
      \draw[HubAccent,line width=0.8pt] ([xshift=18pt]hubepigraph.south west) -- ([xshift=-18pt]hubepigraph.south east);
    \end{tikzpicture}%
  \end{flushright}%
}
\newcommand{\HubQuoteFrame}[1]{%
  \begin{tikzpicture}
    \node[draw=HubLine,line width=0.8pt,fill=HubSurface,inner xsep=32pt,inner ysep=29pt,outer sep=0pt] (hubquote) {#1};
        \fill[HubAccent] ([xshift=-0.4pt,yshift=0.4pt]hubquote.north west) rectangle ([xshift=10.2pt,yshift=-0.4pt]hubquote.south west);
  \end{tikzpicture}%
}

\newenvironment{epigraph}[1][]{%
  \par\medskip\noindent
  \begin{lrbox}{\HubEpigraphBox}%
  \begin{minipage}{0.62\linewidth}%
    \setlength{\parskip}{0.4em}%
    \def\HubTempArg{#1}%
    \ifx\HubTempArg\empty\else{\raggedleft\textcolor{HubAccent}{\footnotesize\bfseries #1}\par}\vspace{0.7em}\fi
    \raggedleft\itshape\small\color{HubStandfirst}\ignorespaces
}{%
  \end{minipage}%
  \end{lrbox}%
  \HubEpigraphFrame{\usebox{\HubEpigraphBox}}%
  \par\medskip
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
  \ifhubgallerywhite
    \par\vspace{8mm}%
  \else
    \par\vspace{35mm}%
  \fi
}
\newsavebox{\HubQuoteBox}
\renewenvironment{quote}[1][]{%
  \def\HubQuoteAttribution{#1}%
  \par\medskip\noindent
  \begin{lrbox}{\HubQuoteBox}%
  \begin{minipage}{\dimexpr\linewidth-64pt\relax}%
    \setlength{\parskip}{0.6em}%
    \fontsize{12.5}{16}\selectfont\bfseries\color{HubQuoteTextColor}\ignorespaces
    \HubQuoteOpenMark
}{%
    \HubQuoteCloseMark
    \ifx\HubQuoteAttribution\empty\else
      \par\vspace{18pt}{\raggedleft\normalfont\small\color{HubMuted}\HubQuoteAttribution\par}\vspace{-10pt}%
    \fi
  \end{minipage}%
  \end{lrbox}%
  \HubQuoteFrame{\usebox{\HubQuoteBox}}%
  \par\medskip
}
\makeatletter
\newenvironment{standfirst}{%
  \ifhubgallerywhite
    \par\medskip\noindent
    \begin{minipage}{0.86\textwidth}%
      \color{HubStandfirst}\itshape\fontsize{12.6}{16.2}\selectfont\ignorespaces
  \else
    \par\smallskip\noindent\color{HubStandfirst}\large\bfseries\ignorespaces
  \fi
}{%
  \ifhubgallerywhite
      \par
    \end{minipage}%
    \par\vspace{8mm}%
  \else
    \par\medskip
  \fi
}
\newenvironment{pullquote}[1][]{%
  \par\medskip\noindent
  \begingroup
  \ifhubgallerywhite
    \begin{center}%
    \begin{minipage}{0.88\linewidth}%
      \raggedright\itshape\fontsize{10.8}{14}\selectfont\color{HubQuoteTextColor}\ignorespaces
  \else
    \raggedright\fontsize{15.5}{21}\selectfont\color{HubQuoteTextColor}\ignorespaces
  \fi
}{%
  \par
  \ifhubgallerywhite
    \end{minipage}%
    \end{center}%
  \fi
  \endgroup
  \par\medskip
}
\makeatother
\newsavebox{\HubCalloutBox}
\newcommand{\HubCalloutFrame}[1]{%
  \begin{tikzpicture}
    \node[draw=HubLine,line width=0.8pt,fill=HubSurface,inner xsep=18pt,inner ysep=12pt,outer sep=0pt] (hubcallout) {#1};
    \fill[HubCalloutEdgeColor] ([xshift=-0.4pt,yshift=0.4pt]hubcallout.north west) rectangle ([xshift=10.2pt,yshift=-0.4pt]hubcallout.south west);
  \end{tikzpicture}%
}
\newenvironment{callout}[1][]{%
  \par\medskip\noindent
  \begin{lrbox}{\HubCalloutBox}%
  \begin{minipage}{\dimexpr\linewidth-36pt\relax}%
    \setlength{\parskip}{0.6em}%
    \def\HubTempArg{#1}%
    \ifx\HubTempArg\empty\else{\raggedright\textcolor{HubCalloutTitleColor}{\textbf{#1}}\par}\vspace{1em}\fi
    \small\color{HubCalloutTextColor}\ignorespaces
}{%
  \end{minipage}%
  \end{lrbox}%
  \HubCalloutFrame{\usebox{\HubCalloutBox}}%
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
    {\centering\textcolor{HubMetricValueColor}{\bfseries\Large #1}\par}
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
  \begin{tcolorbox}[hubcard, colback=HubSurface]
    \def\HubTempArg{#1}%
    \ifx\HubTempArg\empty\else{\raggedright\textcolor{HubCardTitleColor}{\textbf{#1}}\par}\vspace{1em}\fi
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
  \def\HubCodeStringColor{HubStringColor}%
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
  \def\HubCodeStringColor{HubStringColor}%
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
      {HubGraphPrimaryColor, solid},
      {HubStandfirst, dashed},
      {HubLabelBrown, dotted}
    }
  }
}

\newenvironment{graph}[1][]{%
  \par\medskip\noindent
  \begingroup
    \def\HubTempArg{#1}%
    \ifx\HubTempArg\empty\else{\raggedright\textcolor{HubCardTitleColor}{\textbf{#1}}\par}\vspace{1em}\fi
    \centering
}{%
  \par
  \endgroup
  \par\medskip
}

\newenvironment{graph*}[1][]{%
  \begin{figure*}[t]
  \centering
  \begin{graph}[#1]%
}{%
  \end{graph}%
  \end{figure*}%
}

\newenvironment{hubfootnote}{\par\medskip\footnotesize\color{HubMuted}}{\par}

\endinput
