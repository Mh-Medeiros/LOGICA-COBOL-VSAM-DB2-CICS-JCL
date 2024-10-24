       IDENTIFICATION                          DIVISION.
       PROGRAM-ID.                             RELORD.
      *=================================================================*
      *    PROGRAMA   : RELORD
      *    PROGRAMADOR: MATHEUS MEDEIROS
      *    ANALISTA   : IVAN SANCHES
      *    CONSULTORIA: FOURSYS
      *    DATA.......: 07 / 08 / 2024
      *-----------------------------------------------------------------*
      *    OBJETIVO...:  IMPRIMIR UM RELATORIO DOS ASSINANTE
      *        CALSSIFICADOS POR  STREAMING.
      *-----------------------------------------------------------------*
      *    ARQUIVOS                I/O                  INCLUDE/BOOK
      *    CLIENTES               INPUT
      *    RELAT                  OUTPUT
      *
      *
      *
      *-----------------------------------------------------------------*
      *    MODULOS....:
      *
      *-----------------------------------------------------------------*
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
      *-----------------------------------------------------------------*
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO
               "C:\CURSOLOGICA\COBOL\RELATORIOS\CLIENTES2.TXT"
                   FILE STATUS IS FS-CLIENTES.
           SELECT RELATO ASSIGN TO
               "C:\CURSOLOGICA\COBOL\RELATORIOS\RELATO3.TXT"
                   FILE STATUS IS FS-RELATO.
      *=================================================================*
       DATA                                    DIVISION.
      *=================================================================*
      *-----------------------------------------------------------------*
       FILE                                    SECTION.
      *-----------------------------------------------------------------*
      *        OUTPUT -  DADOS DO ARQUIVO DE SAIDA(RELATO)
      *                               LRECL = 86
      *
      *-----------------------------------------------------------------*
       FD  CLIENTES.
       01  REG-CLIENTES.
           05 REG-CODIGO                  PIC X(003)       VALUE SPACES.
           05 REG-NOME                    PIC X(020)       VALUE SPACES.
           05 REG-EMAIL                   PIC X(020)       VALUE SPACES.
           05 REG-STREAM                  PIC X(010)       VALUE SPACES.
           05 REG-ASSINATURA              PIC 9(003)V99    VALUE ZEROS.
      *-----------------------------------------------------------------*
       FD  RELATO.
       01  REG-RELATO                     PIC x(086).

       WORKING-STORAGE                         SECTION.
      *-----------------------------------------------------------------*

      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               "***** INICIO DA WORKING *****".
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               "----- VARIAVEL DE STATUS -----".
      *-----------------------------------------------------------------*
       01  FS-CLIENTES                 PIC 9(002)          VALUE ZEROS.
       01  FS-RELATO                   PIC 9(002)          VALUE ZEROS.
      *-----------------------------------------------------------------*
      *------------------------- DETALHE -------------------------------*
       01  WRK-DETALHE.
           05 WRK-DET-CODIGO           PIC 9(003)          VALUE ZEROS.
           05 FILLER                   PIC X(004)          VALUE SPACES.
           05 WRK-DET-NOME             PIC X(020)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-EMAIL            PIC X(020)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-STREAM           PIC X(010)          VALUE SPACES.
           05 FILLER                   PIC X(002)          VALUE "R$".
           05 WRK-DET-ASSIN            PIC Z.ZZ9,99        VALUE ZEROS.

      *------------------------- CABECALHO 1 ----------------------------*
       01  WRK-CABEC1.
           05 FILLER                    PIC X(015)         VALUE SPACES.
           05 FILLER                    PIC X(020)         VALUE
              "LISTA DE CLIENTES".
           05 FILLER                    PIC X(020)         VALUE SPACES.
           05 FILLER                    PIC X(010)         VALUE
              "PAGINA:".
           05 ACU-PAGINA                PIC 9(003)         VALUE 1.
      *------------------------- CABECALHO 2 ----------------------------*
       01  WRK-CABEC2.
           05 WRK-CODIGO                PIC X(007)         VALUE
              "COD ".
           05 WRK-NOME                  PIC X(021)         VALUE
              "NOME ".
           05 WRK-EMAIL                 PIC X(021)         VALUE
              "EMAIL".
           05 WRK-STREAMING             PIC X(012)         VALUE
              "STREAMING".
           05 WRK-ASSINATURA            PIC X(006)         VALUE
              "VALOR".
      *------------------------- CABECALHO 3 ----------------------------*
       01  WRK-CABEC3.
           05 WRK-BRANCO                PIC X(086)         VALUE SPACES.
      *------------------------- CABECALHO 4 ----------------------------*
       01  WRK-CABEC4.
           05  MSG-SUBTOTAL             PIC X(059)         VALUE
           "SUB-TOTAL DA PAGINA:".
           05 FILLER                    PIC X(002)         VALUE "R$".
           05  ACU-SUBTOTAL-ED          PIC Z.ZZ9,99       VALUE ZEROS.
      *------------------------- CABECALHO 5 ----------------------------*
       01  WRK-CABEC5.
           05  MSG-TOTAL                PIC X(059)         VALUE
           "TOTAL DO RELATORIO:".
           05 FILLER                    PIC X(002)         VALUE "R$".
           05  ACU-TOTAL-ED             PIC Z.ZZ9,99       VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  WRK-CABEC6.
           05 FILLER                    PIC X(050)         VALUE SPACES.
      *-----------------------------------------------------------------*
       01  FILLER                       PIC X(050)         VALUE
               "  VARIAVEIS ACUMULADORAS " .
      *-----------------------------------------------------------------*
       01  ACU-QT-LINHAS                PIC 9(003)  COMP-3 VALUE ZEROS.
       01  ACU-LIDOS                    PIC 9(003)  COMP-3 VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  ACU-SUBTOTAL-AUX             PIC 9(003)V99      VALUE ZEROS.
       01  ACU-TOTAL-AUX                PIC 9(003)V99      VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  WRK-DET-ASSIN-AUX            PIC 9(003)V99      VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  FILLER                       PIC X(050)         VALUE
               " VARIAVEL DE APOIO ".
      *-----------------------------------------------------------------*
       01  WRK-MSG-ERRO                 PIC X(030)         VALUE SPACES.
      *-----------------------------------------------------------------*
       01  WRK-NOME-STREAM              PIC X(012)         VALUE SPACES.
      *-----------------------------------------------------------------*
       01  FILLER                       PIC X(050)         VALUE
           "***** FIM DA WORKING *****".
      *-----------------------------------------------------------------*

      *=================================================================*
       PROCEDURE                               DIVISION.
      *=================================================================*
       0000-PRINCIPAL.

           PERFORM 0100-INICIAR.
           PERFORM 0110-TESTAR-VAZIO.
           PERFORM 0200-PROCESSAR UNTIL FS-CLIENTES  NOT EQUAL 00.
           PERFORM 0300-FINALIZAR.

       0000-PRINCIPAL-FIM.EXIT.
      *-----------------------------------------------------------------*
       0100-INICIAR                            SECTION.
      *-----------------------------------------------------------------*

           OPEN INPUT  CLIENTES
                OUTPUT RELATO.

       0100-INICIAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0110-TESTAR-VAZIO                       SECTION.
      *-----------------------------------------------------------------*

           PERFORM 0120-LER-DADOS.
           IF FS-CLIENTES                  EQUAL  00
               PERFORM 0210-IMP-CABECALHO
               MOVE REG-STREAM TO WRK-NOME-STREAM
           END-IF.

       0110-TESTAR-VAZIO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0120-LER-DADOS                          SECTION.

           READ CLIENTES.

       0120-LER-DADOS-FIM.EXIT.
      *-----------------------------------------------------------------*
       0200-PROCESSAR                          SECTION.
      *-----------------------------------------------------------------

           ADD 1 TO ACU-LIDOS.

           IF  WRK-NOME-STREAM NOT EQUAL REG-STREAM
               PERFORM 0220-IMP-SUBTOTAL
               PERFORM 0210-IMP-CABECALHO

           END-IF

           IF ACU-QT-LINHAS GREATER 12
               PERFORM 0210-IMP-CABECALHO

           END-IF.




           PERFORM 0250-IMP-DETALHE.
           PERFORM 0120-LER-DADOS.

       0200-PROCESSAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0210-IMP-CABECALHO                          SECTION.
      *-----------------------------------------------------------------*
           MOVE ALL "=" TO WRK-CABEC3.



           IF ACU-PAGINA  GREATER THAN  001
               MOVE WRK-CABEC6 TO REG-RELATO
                   WRITE REG-RELATO AFTER PAGE
           END-IF.



           MOVE WRK-CABEC3 TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE

           MOVE WRK-CABEC1 TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE

           MOVE WRK-CABEC3 TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE

           MOVE WRK-CABEC2 TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE

           MOVE WRK-CABEC3 TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE

           MOVE REG-STREAM TO WRK-NOME-STREAM.
           ADD 1  TO ACU-PAGINA .
           MOVE 4 TO ACU-QT-LINHAS.


       0210-IMP-CABECALHO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0220-IMP-SUBTOTAL                       SECTION.
      *-----------------------------------------------------------------*

           MOVE WRK-CABEC3  TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE.

           MOVE WRK-CABEC4  TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE.

           ADD ACU-SUBTOTAL-AUX TO ACU-TOTAL-AUX

           MOVE ACU-TOTAL-AUX   TO ACU-TOTAL-ED

           MOVE ZEROS           TO ACU-SUBTOTAL-AUX.

       0220-IMP-SUB-TOTAL-FIM.EXIT.
      *-----------------------------------------------------------------*
       0250-IMP-DETALHE                        SECTION.
      *-----------------------------------------------------------------*
           MOVE REG-CODIGO       TO WRK-DET-CODIGO.
           MOVE REG-NOME         TO WRK-DET-NOME.
           MOVE REG-EMAIL        TO WRK-DET-EMAIL.
           MOVE REG-STREAM       TO WRK-DET-STREAM.
           MOVE REG-ASSINATURA   TO WRK-DET-ASSIN.
           MOVE REG-ASSINATURA   TO WRK-DET-ASSIN-AUX


           MOVE WRK-DETALHE TO REG-RELATO.
           WRITE REG-RELATO AFTER 1 LINE.



           ADD WRK-DET-ASSIN-AUX TO ACU-SUBTOTAL-AUX.
           MOVE ACU-SUBTOTAL-AUX TO ACU-SUBTOTAL-ED.
           ADD 1 TO ACU-QT-LINHAS.

       0250-IMP-DETALHE-FIM.EXIT.
      *-----------------------------------------------------------------*
       0300-FINALIZAR                          SECTION.
      *-----------------------------------------------------------------*

           IF ACU-LIDOS GREATER THAN 0
               DISPLAY "REGISTROS LIDOS : " ACU-LIDOS
               IF ACU-QT-LINHAS LESS THAN 14
                   PERFORM 0220-IMP-SUBTOTAL
               END-IF
                   PERFORM 0270-IMP-TOTAL
           END-IF.
               CLOSE CLIENTES
                     RELATO.
               GOBACK.

      *-----------------------------------------------------------------*
       0300-FINALIZAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0270-IMP-TOTAL                          SECTION.
      *-----------------------------------------------------------------*

           MOVE WRK-CABEC5      TO REG-RELATO
               WRITE REG-RELATO AFTER 1 LINE.

       0270-IMP-TOTAL-FIM.EXIT.
