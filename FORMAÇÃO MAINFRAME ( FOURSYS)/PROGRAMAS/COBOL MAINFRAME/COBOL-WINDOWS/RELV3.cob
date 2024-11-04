       IDENTIFICATION                          DIVISION.
      * =================================================================*
       PROGRAM-ID.                             RELV3.
      *=================================================================*
      *    PROGRAMA   : RELV3
      *    PROGRAMADOR: MATHEUS H MEDEIROS.
      *    ANALISTA   : IVAN SANCHES
      *    CONSULTORIA: FOURSYS
      *    DATA.......: 31 / 07 / 2024
      *-----------------------------------------------------------------*
      *    OBJETIVO...:
      *
      *-----------------------------------------------------------------*
      *    ARQUIVOS                I/O                  INCLUDE/BOOK
      *    CLIENTES                INPUT
      *
      *
      *-----------------------------------------------------------------*
      *    MODULOS....:
      *
      *-----------------------------------------------------------------*
      *                       * ALTERACOES *                            *
      *-----------------------------------------------------------------*
ALT1  *    PROGRAMADOR: MATHEUS H MEDEIROS
.     *    ANALISTA   : IVAN SANCHES
.     *    CONSULTORIA: FOURSYS
.     *    DATA.......: 31 / 07 / 2024
ATL1  *    OBJETIVO...: INTRODUZIR QUANTIDADE DE PAGINAS
      *
      *-----------------------------------------------------------------*
ALT2  *    PROGRAMADOR: MATHEUS H MEDEIROS
.     *    ANALISTA   : IVAN SANCHES
.     *    CONSULTORIA: FOURSYS
.     *    DATA.......: 01 / 08 / 2024
.     *    OBJETIVO...: INTRODUZIR QUANTIDADE DE REGISTROS LIDOS -
ALT2  *     (NO FINAL DA PAGINA)
      *=================================================================*


      *=================================================================*
       ENVIRONMENT                             DIVISION.
      *=================================================================*


      *-----------------------------------------------------------------*
       CONFIGURATION                           SECTION.
      *-----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------*
       INPUT-OUTPUT                            SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO
               "C:\CURSOLOGICA\COBOL\RELATORIOS\CLIENTESA.DAT"
                  FILE STATUS IS FS-CLIENTES.

      *=================================================================*
       DATA                                    DIVISION.
      *=================================================================*

      *-----------------------------------------------------------------*
       FILE                                    SECTION.
      *-----------------------------------------------------------------*
      *    INPUT -  DADOS DO ARQUIVO DE SAIDA(CLIENTES)
      *                                     LRECL = 58
      *
      *
      *
      *-----------------------------------------------------------------*
       FD  CLIENTES.
       01  REG-CLIENTES.
           05 REG-CODIGO               PIC 9(003)          VALUE ZEROS.
           05 REG-NOME                 PIC X(020)          VALUE SPACES.
           05 REG-EMAIL                PIC X(020)          VALUE SPACES.
           05 REG-STREAM               PIC X(010)          VALUE SPACES.
           05 REG-ASSINATURA           PIC 9(003)V99       VALUE ZEROS.
      *-----------------------------------------------------------------*
       WORKING-STORAGE                         SECTION.
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               "***** INICIO DA WORKING *****".
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " VARIAVEL DE STATUS ".
      *-----------------------------------------------------------------*
       01  FS-CLIENTES                 PIC X(002)          VALUE SPACES.
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " VARIAVEIS DE ACUMULO ".
      *-----------------------------------------------------------------*
       01  WRK-QT-LINHAS               PIC 9(003) COMP-3   VALUE ZEROS.
ATL2   01  ACU-LIDOS                   PIC 9(003) COMP-3   VALUE ZEROS.
ATL1   01  WRK-PAG-AUX                 PIC 9(003) COMP-3        VALUE 1.
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " VARIAVEL ESPELHO ".
      *-----------------------------------------------------------------*
       01  WRK-DETALHE.
           05 WRK-DET-CODIGO           PIC 9(003)          VALUE ZEROS.
           05 FILLER                   PIC X(004)          VALUE SPACES.
           05 WRK-DET-NOME             PIC X(020)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-EMAIL            PIC X(020)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-STREAM           PIC X(010)          VALUE SPACES.
           05 FILLER                   PIC X(002)          VALUE 'R$'.
           05 WRK-DET-ASSINATURA       PIC ZZ9,99          VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " BOOKS DE APOIO ".
      *--------------------------  ERROS  ------------------------------*
       COPY "#BOOKERRO".
      *-------------------------- MSG ERROS ----------------------------*
       COPY "#MSGERRO".

      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " CABECALHOS (1 , 2 E 3 )".
      *-----------------------------------------------------------------*
      *-------------------- CABECALHO 1 --------------------------------*
       01  WRK-CABEC1.
           05 FILLER                   PIC X(025)          VALUE SPACES.
           05 FILLER                   PIC X(030)          VALUE
               " LISTA DE CLIENTES ".
           05 FILLER                   PIC X(020)          VALUE SPACES.
           05 FILLER                   PIC X(008)          VALUE
ALT1           "PAGINA:".
ATL1       05 WRK-PAG                  PIC 9(003)          VALUE 0.
      *--------------------- CABECALHO 2 -------------------------------*
       01  WRK-CABEC2.
           05 FILLER                   PIC X(007)          VALUE
               "CODIGO".
           05 FILLER                   PIC X(021)          VALUE
               "NOME".
           05 FILLER                   PIC X(021)          VALUE
               "EMAIL".
           05 FILLER                   PIC X(011)          VALUE
               "STREAMING".
           05 FILLER                   PIC X(005)          VALUE
               "VALOR".
      *------------------------ CABECALHO 3 ----------------------------*
       01  WRK-CABEC3                  PIC X(086)          VALUE SPACES.


      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           "***** FIM DA WORKING *****".
      *-----------------------------------------------------------------*

      *=================================================================*
       PROCEDURE                               DIVISION.
      *=================================================================*
       0000-PRINCIPAL.

               PERFORM 0100-INICIAR.
               PERFORM 0105-TESTAR-VAZIO.
               PERFORM 0200-PROCESSAR UNTIL FS-CLIENTES NOT EQUAL '00'.
               PERFORM 0300-FINALIZAR.

       0000-PRINCIPAL-FIM.EXIT.

      *-----------------------------------------------------------------*
       0100-INICIAR                            SECTION.
      *-----------------------------------------------------------------*

               OPEN INPUT CLIENTES.
               PERFORM 0104-TESTAR-STATUS.

       0100-INICIAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0104-TESTAR-STATUS                      SECTION.

               IF FS-CLIENTES                     NOT EQUAL '00'
                   MOVE WRK-ERRO-ABERTURA         TO WRK-DESCRICAO-ERRO
                   MOVE FS-CLIENTES               TO WRK-STATUS-ERRO
                   MOVE '0104-TESTAR-STATUS'      TO WRK-AREA-ERRO
                   PERFORM 9999-TRATA-ERRO
               END-IF.

       0104-TESTAR-STATUS-FIM.EXIT.
      *-----------------------------------------------------------------*
       0105-TESTAR-VAZIO                       SECTION.
      *-----------------------------------------------------------------*

               PERFORM 0106-LER-DADOS.

               IF FS-CLIENTES                  NOT EQUAL '00'
                   MOVE WRK-ERRO-ABERTURA      TO WRK-DESCRICAO-ERRO
                   MOVE FS-CLIENTES            TO WRK-STATUS-ERRO
                   MOVE '0105-TESTAR-VAZIO'    TO WRK-AREA-ERRO
                   PERFORM 9999-TRATA-ERRO
               ELSE
                   PERFORM 0210-IMP-CABECALHO
ALT1               MOVE WRK-PAG-AUX TO WRK-PAG
                       DISPLAY WRK-PAG
               END-IF.

       0105-TESTAR-VAZIO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0106-LER-DADOS                          SECTION.
      *-----------------------------------------------------------------*
               READ CLIENTES.

       0106-LER-DADOS-FIM.EXIT.
      *-----------------------------------------------------------------*
       0200-PROCESSAR                          SECTION.
      *-----------------------------------------------------------------*
ATL2           ADD 1 TO ACU-LIDOS.
               IF WRK-QT-LINHAS GREATER 12

                   PERFORM 0210-IMP-CABECALHO

               END-IF.
               PERFORM 0220-IMP-DETALHE.
               PERFORM 0106-LER-DADOS.

       0200-PROCESSAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0210-IMP-CABECALHO                      SECTION.
               MOVE ALL "=" TO WRK-CABEC3.
               DISPLAY " ".
               DISPLAY WRK-CABEC1.
               DISPLAY WRK-CABEC3.
               DISPLAY WRK-CABEC2.
               DISPLAY WRK-CABEC3.
               MOVE 4 TO WRK-QT-LINHAS.

ATL1           ADD 1 TO WRK-PAG-AUX.

       0210-IMP-CABECALHO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0220-IMP-DETALHE                        SECTION.
      *-----------------------------------------------------------------*

               MOVE REG-CODIGO      TO WRK-DET-CODIGO.
               MOVE REG-NOME        TO WRK-DET-NOME.
               MOVE REG-EMAIL       TO WRK-DET-EMAIL .
               MOVE REG-STREAM      TO WRK-DET-STREAM .
               MOVE REG-ASSINATURA  TO WRK-DET-ASSINATURA.

               DISPLAY WRK-DETALHE.

               ADD 1 TO WRK-QT-LINHAS.

       0220-IMP-DETALHE-FIM.EXIT.
      *-----------------------------------------------------------------*
       0300-FINALIZAR                          SECTION.
      *-----------------------------------------------------------------*
               DISPLAY WRK-CABEC3.

ATL2           DISPLAY "TOTAL DE REGISTROS LIDOS :" ACU-LIDOS.

               CLOSE CLIENTES.
               GOBACK.
      *-----------------------------------------------------------------*
       0300-FINALIZAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       9000-MSG-ERRO                           SECTION.

               DISPLAY "===== ERRO NO PROGRAMA ====="
               DISPLAY "MENSSAGEM....:"    WRK-DESCRICAO-ERRO.
               DISPLAY "FILE STATUS..:"    WRK-STATUS-ERRO.
               DISPLAY "AREA / SECAO.:"    WRK-AREA-ERRO.

       9000-MSG-ERRO-FIM.EXIT.
      *-----------------------------------------------------------------*
       9999-TRATA-ERRO                         SECTION.
      *-----------------------------------------------------------------*

              DISPLAY WRK-MSG-ERROS.
              GOBACK.
       9999-TRATA-ERRO-FIM.EXIT.
