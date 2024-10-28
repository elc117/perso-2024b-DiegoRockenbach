# Produção personalizada em Haskell - Trabalho 1
***Diego Rockenbach; Sistemas de Informação, 3°sem - 2024/2***

## Análise inicial da ideia do trabalho
A ideia inicial do trabalho nasceu como sendo uma implementação do jogo [Artigo.app](https://artigo.app/), porém após começar a programação percebi que ele iria requerer mais conhecimento de Haskell do que eu possuía, sem falar na praticamente essencial integração com uma biblioteca gráfica própria. Após desistir da ideia, conversei com meu colega Diogo, que também havia recentemente desistido de seu projeto passado e me informou que a professora havia sugerido que ele implementasse um Campo Minado (Minesweeper), mas ele não se interessou pela ideia. Coincidentemente, a ideia de implementar um campo minado parecia mais simples e divertida para mim. Assim, fizemos uma troca de projetos: ele decidiu implementar o Artigo.app, enquanto eu decidi implementar um Campo Minado.

Pensando na implementação de um jogo de Campo Minado em Haskell, me ocorreram alguns conhecimentos na linguagem que seriam necessários para conseguir fazer essa implementação um projeto funcional e relativamente fidedigno. Seria necessário aprender e implementar entrada e saída de dados e seu tipo IO, para possibilitar a influência do usuário no jogo, manipulação de matrizes (listas de listas), algum tipo de randomicidade para geração e posicionamento das minas no tabuleiro e um loop de recursão para permitir que o usuário escolha determinada casa e o programa atualize visualmente o tabuleiro, verifique se aquilo era uma bomba e se não fosse permitir ao usuário sua próxima jogada. Todos as funções acima eram coisas novas para mim, visto que não havia implementado nada disso nos exercícios passados em aula, e nada na mesma escala também. Dito isso, a lógica do jogo estava basicamente pronta em minha mente, pois eu havia considerado tudo de uma perspectiva de uma linguagem imperativa (como C ou PHP). Assim, o verdadeiro problema seria entender como converter a lógica de uma linguagem imperativa para uma funcional, e aprender como isso iria funcionar em Haskell. Portanto, embora eu soubesse que era uma proposta desafiadora na qual eu dependeria das mais diversas fontes externas para conseguir aprender a circunventar a questão citada acima, eu também sabia que eu aprenderia muito durante o desenvolvimento, e isto me instigava.

## Desenvolvimento
Antes de qualquer coisa eu precisava descobrir como funcionavam matrizes em Haskell e como eu poderia manipulá-las em meu programa. Eu já havia decidido que a matrix do tabuleiro seria uma lista de lista de tuplas de inteiros, em que o primeiro inteiro de cada tupla seria referente a que tipo de casa era aquela (casa não revelada/casa revelada vazia/casa com bomba, etc) e o segundo inteiro de cada tupla seria referente a quantidade de bombas localizados nos 8 quadrados adjacentes da casa em questão. Assim, as primeiras funções criadas foram `criarTabuleiro` e `imprimirTabuleiro`. Para implementar essas funções eu achei antes necessário criar um tipo de dados dedicado para o tabuleiro, pois é assim que eu o faria em C (com um struct), apesar de que repensando o código agora talvez não fosse necessário. Com a função de imprimir tabuleiro corretamente implementada eu poderia finalmente testar o código e após resolver alguns problemas tudo estava funcionando corretamente, mas a maior parte do programa ainda está ausente, visto que a função de criar o tabuleiro apenas o cria vazio, sem nenhuma mina inserida, sem contar que ainda faltava programar toda a interação com o usuário.

Depois de pesquisar descobri que Haskell possui uma biblioteca dedicada para funções de randomicidade (`System.Random (randomRIO)`) e com ela eu criei a função `gerarCoordenadasRandom`, que é chamada pela função `gerarMinas` juntamente com a função `inserirMina`. O funcionamento é o seguinte: a função `gerarMinas` é chamada no início do programa, logo após a criação do tabuleiro ocorrer. Ela recebe como parâmetro o tamanho do tabuleiro e também a quantidade de minas que desejam ser inseridas, e este número será o limitador de quantas vezes ela irá chamar a função para gerar coordenadas aleatórias, de modo que a função `gerarCoordenadasRandom` retorna uma lista de coordenadas _IO_, e em cada elemento desta lista será aplicada a função `inserirMina` através do comando `foldl`, alterando o primeiro elemento das tuplas de inteiros e transformando-o em 2 (casa com mina não revelada).

Após isso, foram implementadas as funções `atualizarTabuleiro` e `contarMinasAdjacentes`, novamente com a mesma lógica de abstração aplicada anteriormente: a função `atualizarTabuleiro` percorre todo o tabuleiro (com as minas já inseridas) utilizando dois `list comprehensions` aninhados e checa casa por casa se esta contém uma mina (primeiro elemento da tupla = 2), e, caso não contenha, ele chama a função `contarMinasAdjacentes` (uma vez por casa), que percorre os 8 adjacentes de cada casa, utiliza a função nativa `map` para checar se aquela casa contém uma bomba (_map_ = 1) ou não (_map_ = 0), e após isso soma o resultado de todos os _maps_ para saber o número de minas adjacentes. Com todas as funções até então comentadas testadas e funcionando corretamente, restava apenas implementar a interação com o usuário e o _loop_ básica de jogo, que verificaria se o usuário havia escolhido uma casa com minas ou não e atualizaria o tabuleiro com os novos valores da casa revelada, visto que até então eu estava testando o programa executando as funções uma por uma, na ordem correta, ao invés de apenas chamar a função `main` já com o loop implementado.

Para resolver esse problema eu parti para implementar a função `main`, que seria responsável apenas por chamar as funções `criarTabuleiro`, `gerarMinas` e `atualizarTabuleiro`, nesta ordem, para, respectivamente, criar a matriz com o número x de casas, inserir as minas na matrix do tabuleiro criado (popular o primeiro elemento de cada tupla) e calcular os adjacentes de cada uma das casas (popular o segundo elemento de cada tupla), para então chamar a função do loop principal do programa, `jogar`. Esta função que iria chamar a função `imprimirTabuleiro`, então receber as coordenadas input do usuário e através da função `selecionaCasa` definir qual tipo de casa era aquela: se a casa selecionada era uma bomba o primeiro valor da tupla seria redefinido de 2 para 3 (a ser usado na próxima função), e, caso a casa não fosse uma bomba, o valor seria redefinido de 0 (casa vazia não revelada) para 1 (casa revelada vazia), relevante para a função `imprimirTabuleiro` mostrar o número de adjacentes somente de casas vazias já reveladas. Após o tabuleiro receber seu novo valor, a função `checaTabuleiroGameOver` é chamada, que irá simplesmente percorrer todo o tabuleiro para verificar se o primeiro elemento de alguma das tuplas é 3. Caso for, isso quer dizer que aquela casa continha uma bomba e foi selecionada, e, portanto, o jogo deve acabar (_estadoDeJogo_ vira 0). Caso a função não encontre nenhum 0, _estadoDeJogo_ recebe 1 e a função `jogar` se chama recursivamente.

### Principais dificuldades encontradas
- Validar uma ideia para o início do trabalho;
- Aprender como um loop de jogo poderia funcionar em Haskell;
- Converter em minha cabeça a lógica programacional em uma linguagem imperativa para uma linguagem funcional;
- Encontrar fontes confiáveis para começar o programar e resolver erros (a documentação oficial de Haskell é pobre);
- Entender o que estava acontecendo no código e por que havia sido feito daquele modo, quando a resolução vinha de ferramentas de IA.

## Vídeo desmontração do código rodando

[![Watch the video](https://raw.githubusercontent.com/elc117/perso-2024b-DiegoRockenbach/main/demo_video.png)](https://raw.githubusercontent.com/elc117/perso-2024b-DiegoRockenbach/main/demo_video.mp4)

## Referências (em ordem de relevância)

> Ferramentas de Inteligência Artificial

> [Stack Overflow](https://stackoverflow.com/)

> [r/Haskell](https://www.reddit.com/r/haskell/)

> Discutir projetos paralelos entre colegas