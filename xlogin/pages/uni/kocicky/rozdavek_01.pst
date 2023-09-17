---
preset: kocicky
geometry: margin=1cm
output: pdf_document
header-includes: |
            \usepackage{tikz-cd}
            \usepackage{adjustbox}
            \pagenumbering{gobble}
            \usepackage[labelformat=empty]{caption}

---

## Seminár 1: Úvod do teórie kategórií

### Kategória - $C(O, A, \circ)$:

- $O$ - kolekcia objektov
- $A$ - kolekcia _arrows_ - morfizmy medzi objektami z $O$
- $\circ$ - binárne skladanie morfizmov 
    - musí platiť asociativita - $(h \circ g) \circ f = h \circ (g \circ f) = h \circ g \circ f$
    - identita $id$ je voči operátoru neutrálna aj zľava aj sprava, nech $f : a \to b$ potom $f \circ id_a = id_b \circ f = f$
- $id$ / $1$ - identické morfizmy pre všetky objekty ($id_a$ - identický morfizmus pre $a \in O$)
- $Hom(a, b)$ - kolekcia morfizmov **z** $a$ **do** $b$

### Kategórie

- _Malá_: aj $O$ aj $A$ sú množiny
- _Veľká_: nie malá
- _Lokálne malá_: pre všetky objekty $a, b \in C$ je $Hom(a, b)$ množina
- _Riedka (thin)_: pre všetky objekty $a, b \in C$ platí $|Hom(a, b)| \in \{0, 1\}$
- _Voľná (free)_: kategória generovaná z orientovaného grafu pomocou voľnej konštrukcie (doplnenie identít a zložením hrán - pre každú cestu z $u$ do $v$ existuje hrana reprezentujúca túto cestu)

### Špeciálne objekty

- *iniciálny objekt* 
    - $a$ je iniciálny objekt $\iff \forall b \in O_C \ldotp\exists! m \in A_C \ldotp m: a \rightarrow b$ 
    - do každého objektu kategórie vedie unikátna šípka
    - v kategórii "typov" (Set) si môžeme predstaviť `void`, pretože pre každý iný typ `T` existuje funkcia `asburd : void -> T`
    - unikátny až na unikátny izomorfizmus

- *terminálny objekt* (duálny k iniciálnemu objektu)
    - $a$ je terminálny objekt $\iff \forall b \in O_C \ldotp\exists! m \in A_C \ldotp m : b \rightarrow a$ 
    - z každého objektu kategórie vedie unikátna šípka
    - v kategórii "typov" si môžeme predstaviť `unit`, pretože pre každý iný typ existuje funkcia `unit : T -> ()`
    - unikátny až na unikátny izomorfizmus

*Opačná kategória* $C^{op}$ - Kategória s opačnými morfizmami (od $C$). Môžme hovoriť o duálnych pojmoch (napr. terminálny objekt je iniciálny v opačnej kategórii, mono je duálne s epi, atď).

### Špeciálne morfizmy

- _monomorfizmus (mono)_

    - $f : a \to b \in C$ je monomorfizmus $\iff$ 
    
    > $\forall x \in C \ldotp \forall g, h : x \to a \in C \ldotp \{ f \circ g = f \circ h \implies g = h \}$ 
    
    - v **Set** je morfizmus $f$ mono $\iff$ funkcia $f$ je injektívna

- _epimorfizmus (epi)_ (duálny k monomorfizmu)

    - $f : a \to b \in C$ je epimorfizmus $\iff$

    > $\forall x \in C \ldotp \forall g, h : b \to x \in C \ldotp \{ g \circ f = h \circ f \implies g = h \}$ 

    - v **Set** je morfizmus $f$ epi $\iff$ funkcia $f$ je surjektívna
- _izomorfizmus_

    > $f : a \to b \in C$ je izomorfizmus $\iff$ 

    > $\exists g : b \to a \in C \ldotp \{ g \circ f = id_a \land f \circ g = id_b \}$

- _endomorfizmus_

    > $f : a \to a$
    
- _automorfizmus_

    > endomorfizmus + izomorfizmus

### Príklady kategórií: 

- **0** - kategória bez prvkov a morfizmov
- **1** - kategória s práve jedným prvkom a jeho identitou
- **2** - kategória s práve dvoma prvkami, medzi ktorými sú morfizmy 
- **Set** - kategória množín a funkcií 
- **Grp** - kategória grúp a homomorfizmov
- **Cat** - kategória malých kategórií a funktorov
- _Order_ - kategória usporiadania s $\le$
- _Voľná kategória_ - konštrukcia z orientovaného grafu 
- _Monoid ako kategória_ - kategória s jedným objektom a endomorfizmami 
- _Grupa ako kategória_ - kategória s jedným objektom a automorfizmami
- **Hask** - "kategória typov v jazyku Haskell a funkcií"

\vspace{5em}

\begin{tikzcd}[row sep=large]
     & & void \arrow[ld, "absurd_{int}"', bend left] \arrow[rd, "absurd_{bool}", bend right] \arrow[loop, distance=2em, in=125, out=55] \arrow[rrdddd, "absurd_{unit}", bend left=49] \arrow[lldddd, "absurd_{kocicka}"', bend right=49] & & \\
     & int \arrow[rr, "isEven"] \arrow[loop, distance=2em, in=215, out=145] \arrow[rrrddd, "unit_{int}"] \arrow[lddd, "kocicka_{int}"'] & & bool \arrow[loop, distance=2em, in=35, out=325] \arrow[rddd, "unit_{bool}"] \arrow[lllddd, "kocicka_{bool}"] & \\
     & & & & \\
     & & & & \\
    kocicka \arrow[rrrr, "unit_{kocicka}", bend right] \arrow[loop, distance=2em, in=215, out=145] & & & & unit \arrow[llll, "kocicka_{unit}"] \arrow[loop, distance=2em, in=35, out=325]
\end{tikzcd}

### Zdroje:

Milewski, B., 2022. Category Theory for Programmers: The Preface (Part One: 1-4). [online] Bartosz Milewski's Programming Cafe. Available at: <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/> [Accessed 3 March 2022].

Buurlage, J-W, 2022. Categories and Haskell (Chapter 1). [online] Github. Available at: <https://github.com/jwbuurlage/category-theory-programmers/> [Accessed 3 March 2022].


