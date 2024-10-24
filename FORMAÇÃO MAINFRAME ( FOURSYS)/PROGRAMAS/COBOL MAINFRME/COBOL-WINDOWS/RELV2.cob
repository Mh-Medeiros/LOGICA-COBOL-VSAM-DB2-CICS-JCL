       IDENTIFICATION                          DIVISION.
      *=================================================================*
       PROGRAM-ID.                             RELV2.
      *=================================================================*
      *    PROGRAMA   : RELV2
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
      *
ALT1  *    PROGRAMADOR: MATHEUS H MEDEIROS
.     *    ANALISTA   : IVAN SANCHES
.     *    CONSULTORIA: FOURSYS
.     *    DATA.......: 31 / 07 / 2024
ALT1  *    OBJETIVO...: INTRODUZIR QUANTIDADE DE PAGINAS
      *
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
               "C:\CURSOLOGICA\COBOL\RELATORIOS\CLIENTES.DAT"
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
       01  WRK-QT-LINHAS               PIC 9(003)          VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " VARIAVEL ESPELHO ".
      *-----------------------------------------------------------------*
       01  WRK-DETALHE.
           05 WRK-DET-CODIGO           PIC 9(003)          VALUE ZEROS.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-NOME             PIC X(020)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-EMAIL            PIC X(020)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-STREAM           PIC X(010)          VALUE SPACES.
           05 FILLER                   PIC X(001)          VALUE SPACES.
           05 WRK-DET-ASSINATURA       PIC 9(003)V99       VALUE ZEROS.
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
ATL1       05 FILLER                   PIC X(008)          VALUE
               "PAGINA:".
ATL1       05 WRK-PAG                  PIC 9(003)          VALUE 1.
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
       01  WRK-CABEC3                  PIC X(080)          VALUE SPACES.
      *     "==========================================================".

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

       0100-INICIAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0105-TESTAR-VAZIO                       SECTION.
      *-----------------------------------------------------------------*

               PERFORM 0106-LER-DADOS.

               IF FS-CLIENTES NOT EQUAL '00'
                   DISPLAY "ARQUIVO CLIENTES VAZIO"
               ELSE
                   PERFORM 0210-IMP-CABECALHO

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

               IF WRK-QT-LINHAS GREATER 12
                   PERFORM 0210-IMP-CABECALHO
               END-IF.
               PERFORM 0220-IMP-DETALHE.
               PERFORM 0106-LER-DADOS.

       0200-PROCESSAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0210-IMP-CABECALHO                      SECTION.

               DISPLAY  " ".
               DISPLAY WRK-CABEC1.
               MOVE ALL "=" TO WRK-CABEC3.
                   DISPLAY WRK-CABEC3.
               DISPLAY WRK-CABEC2.
               MOVE ALL "=" TO WRK-CABEC3.
                   DISPLAY WRK-CABEC3.

               MOVE 4 TO WRK-QT-LINHAS.
ATL1           ADD 1 TO WRK-PAG.

       0210-IMP-CABECALHO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0220-IMP-DETALHE                        SECTION.
      *-----------------------------------------------------------------*
               MOVE REG-CODIGO     TO WRK-DET-CODIGO.
               MOVE REG-NOME       TO WRK-DET-NOME.
               MOVE REG-EMAIL      TO WRK-DET-EMAIL .
               MOVE REG-STREAM     TO WRK-DET-STREAM .
               MOVE REG-ASSINATURA TO WRK-DET-ASSINATURA.

               DISPLAY WRK-DETALHE.

               ADD 1 TO WRK-QT-LINHAS.

       0220-IMP-DETALHE-FIM.EXIT.
      *-----------------------------------------------------------------*
       0300-FINALIZAR                          SECTION.
      *-----------------------------------------------------------------*

               CLOSE CLIENTES.
               GOBACK.
      *-----------------------------------------------------------------*
       0300-FINALIZAR-FIM.EXIT.
      *-----------------------------------------------------------------*
