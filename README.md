# emascii

Editor de desenhos ASCII para Emacs: canvas sem limites, seleção por forma
ou célula a célula, peças flutuantes para mover e colar, galeria de
desenhos reutilizáveis e cópia pronta para colar em qualquer editor.

## Instalação

```elisp
(use-package emascii
  :load-path "~/dev/emascii"
  :commands (emascii-mode emascii-new emascii-gallery emascii-gallery-insert))
```

Abra um canvas novo com `M-x emascii-new`, ou ative `M-x emascii-mode` em
qualquer arquivo `.txt`.

## Atalhos

### Canvas

| Tecla | Ação |
|---|---|
| setas, `C-f` `C-b` `C-n` `C-p` | anda livremente; cria espaços e linhas quando precisa |
| digitar | sobrescreve a célula, nunca empurra o texto |
| `RET` | desce uma linha, na coluna onde você começou a digitar |
| `DEL` / `C-d` | apaga (vira espaço) sem mover o resto |
| `S-DEL` / `S-<delete>` | apaga e puxa o resto da linha uma casa para a esquerda, como num editor normal |
| `C-k` | apaga até o fim da linha, sem juntar linhas |
| `TAB` | anda até a próxima parada de tab |
| clique | põe o cursor em qualquer célula, mesmo depois do fim da linha |

### Seleção

| Tecla | Ação |
|---|---|
| arrastar com o mouse | seleciona com a forma atual |
| `S-arrastar` | adiciona outra forma à seleção |
| `C-clique` ou `M-clique` | marca/desmarca uma célula |
| `S-setas` | seleciona com a forma atual pelo teclado |
| `C-SPC` | começa uma forma no cursor; de novo, fixa a forma e permite começar outra |
| `C-c C-t` | marca/desmarca a célula do cursor |
| `C-c C-o` | troca a forma: retângulo, elipse, laço (contorno livre) |
| `C-g` | limpa a seleção |

No laço, o contorno é o caminho que o cursor ou o mouse percorreu.

### Recortar, copiar, mover, colar

Funciona com o `cua-mode` ligado (`C-c` `C-x` `C-v`) e também com
`M-w` `C-w` `C-y`.

| Tecla | Ação |
|---|---|
| `C-c` / `M-w` | copia a seleção |
| `C-x` / `C-w` | recorta: copia e deixa espaço em branco no lugar |
| `C-c C-m` | levanta a seleção para mover |
| `C-v` / `C-y` | cola o clipboard como peça flutuante no cursor |
| `C-c C-k` / `DEL` | apaga a seleção |
| `C-c C-c` | copia o canvas inteiro pronto para exportar (sem `cua-mode`, copia a seleção se houver) |

### Peça flutuante

Enquanto flutua, o fundo não é alterado. Os espaços da peça são
transparentes.

| Tecla | Ação |
|---|---|
| setas / arrastar com o mouse | move 1 célula |
| `S-setas` | move 8 células (`emascii-big-step`) |
| `t` | alterna espaços transparentes/opacos |
| `RET` / duplo clique | solta a peça |
| `C-g` / `ESC` | cancela e restaura tudo |

Qualquer outro comando solta a peça antes de rodar. Soltar vira um único
passo de `undo`.

### Galeria

| Tecla | Ação |
|---|---|
| `C-c C-s` | salva a seleção (ou o canvas inteiro) na galeria |
| `C-c C-i` | insere um desenho buscando pelo nome (com o `vertico`, mostra o tamanho) |
| `C-c C-g` | abre a galeria com miniaturas, agrupada por pasta |

Na galeria: `RET`/clique insere no canvas, `/` busca pelo nome, `n` `p`
navegam, `e` edita, `r` renomeia ou move (`pasta/nome`), `d` apaga,
`+` cria pasta, `g` atualiza, `q` fecha.

Os desenhos são arquivos `.txt` em `~/.emacs.d/emascii-gallery/`
(`emascii-gallery-directory`); subpastas são as pastas da galeria.

## Exportar

Copiar (`C-c C-c`, `C-c`, `M-w`) recorta o desenho até a menor caixa que
o contém, troca tabs por espaços e completa todas as linhas até a mesma
largura. No macOS o clipboard recebe três versões: texto puro, HTML e RTF
com fonte monoespaçada (`emascii-export-font`, padrão Menlo 10pt), para o
Word e similares manterem o alinhamento. Desligue com
`(setq emascii-rich-clipboard nil)`.

## Testes

```sh
make test EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
```

## Decisões que tomei (para revisar)

- Emacs Lisp puro, sem dependências; exige Emacs 29.1+. Testado no 30.2
  gráfico do macOS.
- Andar pelo canvas cria espaços de verdade no buffer. Eles são removidos
  quando você sai da linha e ao salvar. Por isso o buffer fica marcado
  como modificado só de andar. O `undo` junta esses espaços com a próxima
  edição, para não virar passos extras.
- Ao salvar, espaços no fim das linhas e linhas vazias no fim do arquivo
  são removidos.
- Tabs são convertidos em espaços ao ativar o modo.
- Todo caractere conta como uma coluna. Caracteres largos (emoji, CJK)
  desalinham o canvas.
- As miniaturas da galeria cortam o desenho (até 28x8, `~` e `...` marcam
  o corte) em vez de reduzi-lo, porque arte ASCII reduzida fica ilegível.
- A transparência considera só o espaço. Não existe "espaço opaco"
  salvo dentro de uma peça; a tecla `t` deixa a peça inteira opaca.
- `RET` não quebra a linha: só desce o cursor.
- O clipboard rico usa `osascript` e só existe no macOS. Em outros sistemas
  vai só o texto puro.
- Sem ferramentas de desenho (caixas, linhas, setas), como combinado.

## Ainda não verificado

- Colar no Word de verdade. Verifiquei que o clipboard recebe texto,
  HTML e RTF com a fonte certa, mas não abri o Word.
- O uso interativo no seu Emacs com a sua configuração. Os testes
  automáticos rodam com `emacs -Q`; o arrasto com o mouse foi simulado
  num Emacs gráfico.
