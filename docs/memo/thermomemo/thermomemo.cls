\ProvidesClass{thermomemo}[Memos for thermopack]
% Use ISO format for dates
\RequirePackage[iso]{isodate}

% Language options
\LoadClass[a4paper,11pt]{article}
\DeclareOption{twocolumn}{\OptionNotUsed}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax

% Set margins
\RequirePackage[top=3.1cm,      bottom=15ex,
                left=2.3cm,     right=2.3cm,
                headheight=1cm, footskip=2.5ex]{geometry}

\newcommand{\MainLogo}{../thermomemo/thermotools_logo.png}
\newcommand{\Website}{https://thermotools.github.io/thermopack/index.html}
\newcommand*{\Institute} {}
\newcommand*{\Location}  {}
\newcommand*{\institute}[1]{\renewcommand{\Institute}{#1}}
\newcommand*{\location}[1]{\renewcommand{\Location}{#1}}

\institute{Thermotools}
\location{%
  Trondheim\\NORWAY
}

\NeedsTeXFormat{LaTeX2e}
\RequirePackage{xcolor}
\definecolor{thermotoolsblue}{cmyk/RGB/HTML}{1,0.57,0,0.40/0,68,124/00447C}
\definecolor{thermotoolsdarkgrey}{cmyk/RGB/HTML}{0,0.09,0.16,0.43/161,149,137/A19589}
\colorlet{thermotoolsdarkgray}{thermotoolsdarkgrey}

% The information box
\RequirePackage{hyperref}
\newcommand{\Information}[1][thermotoolsblue]{%
  \noindent%
  \footnotesize%
  \raggedright%
  \setlength{\parskip}{1ex}%
  \color{#1}%
  \textbf{\Institute}\\
  Location: \\ \Location \\
  \vspace{2mm}
  \url{\Website}

}

% Setting up header
\RequirePackage{nccfancyhdr,lastpage}
\pagestyle{fancy}
\newpagestyle{firstpage}[fancy]{%
  \fancyhead[l]{\includegraphics[width=0.08\textwidth]{\MainLogo}\hspace{0.8em}%
  \raisebox{0.5\height}{\textbf{\textsf{\Huge Thermotools memo}}}}%
}

% Header
\renewcommand{\headrulewidth}{0pt}
  \fancyhead[c]{\includegraphics[width=0.04\textwidth]{\MainLogo}\hspace{0.5em}%
  \raisebox{0.5\height}{\textbf{\textsf{\Large Thermotools memo}}}}%

% Footer
\renewcommand{\footrulewidth}{0pt}
\fancyfoot[c]{\sffamily\thepage}

% This separating line is used across several documents,
% for example in footers.
\RequirePackage{tikz}
\newcommand{\DashedRule}{%
  \begin{tikzpicture}%
    \draw[color=thermotoolsdarkgray, line width=0.5pt, dashed]
         (0,-1ex) (0,1ex) (0,0) -- (0.999\textwidth,0);%
  \end{tikzpicture}%
}

\RequirePackage{tabularx}
\newcommand{\LabelText}{\small\textbf}
\newcommand{\frontmatter}{%
  \thispagestyle{firstpage}%
  \sffamily%
  \noindent%
  \begin{minipage}[b]{0.7\textwidth}
    \setlength{\parskip}{2ex}%
    %\Huge Memo

    \huge\textbf \@title
  \end{minipage}
  \hfill
  \begin{minipage}[b]{0.25\textwidth}
    \vspace*{-25pt}%
    \footnotesize
    \Information
  \end{minipage}

  \vspace{4ex}%
  \noindent%
  \DashedRule\\
  \begin{minipage}{0.75\textwidth}
    \begin{tabularx}{\textwidth}{X}
      \LabelText AUTHOR \\
      \@author
    \end{tabularx}
  \end{minipage}
  \begin{minipage}{0.25\textwidth}
    \LabelText DATE \\
    \@date
  \end{minipage}
  \DashedRule\\
  \rmfamily
  \noindent
}

\newcommand{\mainmatter}{}

\newcommand{\backmatter}{}
