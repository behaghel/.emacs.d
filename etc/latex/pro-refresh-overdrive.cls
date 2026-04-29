%%% pro-refresh-overdrive.cls --- Org export flagship class

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pro-refresh-overdrive}[2026/04/29 Flagship Org export class]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass{article}

\RequirePackage{geometry}
\geometry{margin=15mm}
\RequirePackage{xcolor}
\RequirePackage{afterpage}
\RequirePackage{array}
\RequirePackage{tabularx}
\RequirePackage{colortbl}
\RequirePackage{tikz}
\RequirePackage{graphicx}

\definecolor{HubPaper}{HTML}{FCF5EE}
\definecolor{HubInk}{HTML}{0C3035}
\definecolor{HubAccent}{HTML}{FF550F}
\definecolor{HubMuted}{HTML}{543F21}
\definecolor{HubLine}{HTML}{D8B5A4}
\definecolor{HubSurface}{HTML}{FFF4EC}

\AtBeginDocument{\color{HubInk}}
\AtBeginDocument{\hypersetup{colorlinks=true,linkcolor=HubAccent,urlcolor=HubAccent}}

\renewcommand{\familydefault}{\sfdefault}
\pagestyle{empty}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.6em}
\setlength{\tabcolsep}{5pt}
\renewcommand{\arraystretch}{1.2}
\arrayrulecolor{HubLine}

\makeatletter
\newcommand{\HubExportEyebrowBlock}{}
\providecommand{\HubHeroPatternGraphic}{\includegraphics[width=118mm]{hero-pattern.png}}
\providecommand{\HubHeroLogoGraphic}{\includegraphics[width=32mm]{hero-logo.pdf}}
\newcommand{\HubMetaSeparator}{\hspace{0.8em}{\color{HubLine}\textbullet}\hspace{0.8em}}
\renewcommand{\thesection}{\ifnum\c@section<10 0\fi\arabic{section}}
\def\@seccntformat#1{{\color{HubAccent}\csname the#1\endcsname}\hspace{0.75em}}
\newcommand{\HubSectionFormat}[1]{#1\par\vspace{0.5ex}{\color{HubMuted}\rule{\linewidth}{0.8pt}}}
\renewcommand\section{\@startsection{section}{1}{\z@}{2.8ex}{1.1ex}{\normalfont\Large\bfseries\raggedright\color{HubInk}\HubSectionFormat}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}{2.2ex}{0.9ex}{\normalfont\large\bfseries\raggedright\color{HubMuted}}}
\makeatother

\newenvironment{hubhero}{%
  \par\noindent
  \begin{tikzpicture}[overlay]
    \fill[HubPaper] (-15mm, 15mm + \topskip) rectangle (\paperwidth - 15mm, 15mm + \topskip - \paperheight);
    \coordinate (CardNW) at (-15mm, 15mm + \topskip);
    \coordinate (CardNE) at (\textwidth + 15mm, 15mm + \topskip);
    \node[anchor=north west, opacity=0.18, inner sep=0pt] at (CardNW) {\HubHeroPatternGraphic};
    \node[anchor=north east, inner sep=0pt] at (CardNE) [xshift=-15mm, yshift=-19mm] {\HubHeroLogoGraphic};
  \end{tikzpicture}%
  \par\noindent
}{%
  \par\vspace{1.2em}
}
\renewenvironment{quote}{\par\medskip\noindent\color{HubInk}\itshape\large}{\par\medskip}
\newenvironment{standfirst}{\par\medskip\noindent\color{HubMuted}\large\bfseries\ignorespaces}{\par\medskip}
\newenvironment{pullquote}[1][]{\def\HubPullquoteAttribution{#1}\par\medskip\noindent\itshape\Large\color{HubInk}}{\par\ifx\HubPullquoteAttribution\empty\else{\raggedleft\normalfont\small\color{HubMuted}\HubPullquoteAttribution\par}\fi\medskip}
\newenvironment{callout}[1][]{\par\medskip\noindent\setlength{\fboxsep}{8pt}\fcolorbox{HubLine}{HubSurface}{\begin{minipage}{0.94\linewidth}\ifx\relax#1\relax\else{\color{HubMuted}\bfseries\small\MakeUppercase{#1}\par\smallskip}\fi\small\color{HubInk}\ignorespaces}}{\end{minipage}\par\medskip}
\newenvironment{metrics}{\par\medskip\noindent}{\par\medskip}
\newenvironment{metric}[1][]{\begin{minipage}[t]{0.235\linewidth}\raggedright\textcolor{HubAccent}{\bfseries\Large #1}\par\small\color{HubMuted}\ignorespaces}{\par\end{minipage}\hfill}
\newenvironment{pillars}{\par\medskip\noindent}{\par\medskip}
\newenvironment{pillar}[1][]{\begin{minipage}[t]{0.48\linewidth}\setlength{\fboxsep}{6pt}\fcolorbox{HubLine}{white}{\begin{minipage}[t]{0.9\linewidth}\textbf{#1}\par\smallskip\small\color{HubInk}\ignorespaces}}{\end{minipage}\end{minipage}\hfill}
\newenvironment{hubcode}{\par\medskip\noindent{\color{HubAccent}\rule{\linewidth}{0.8pt}}\par\smallskip\ttfamily\small\color{HubInk}}{\par\smallskip\noindent{\color{HubAccent}\rule{\linewidth}{0.8pt}}\par\medskip}
\newenvironment{hubfootnote}{\par\medskip\footnotesize\color{HubMuted}}{\par}

\endinput
