       IDENTIFICATION                          DIVISION.
      *=================================================================*
       PROGRAM-ID.                             RELASSIN.

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
               "C:\CURSOLOGICA\COBOL\RELATORIOS\CLIENTES.DAT"
                   FILE STATUS IS FS-CLIENTES.

      *=================================================================*
       DATA                                    DIVISION.
      *=================================================================*
      *-----------------------------------------------------------------*
       FILE                                    SECTION.
      *-----------------------------------------------------------------*
      *        INPUT -  DADOS DO ARQUIVO DE SAIDA(CLIENTES)
      *                               LRECL = 58
      *-----------------------------------------------------------------*
       FD  CLIENTES.
       01  REG-CLIENTES.
           05 REG-CODIGO                  PIC X(003)       VALUE SPACES.
           05 REG-NOME                    PIC X(020)       VALUE SPACES.
           05 REG-EMAIL                   PIC X(020)       VALUE SPACES.
           05 REG-STREAM                  PIC X(010)       VALUE SPACES.
           05 REG-ASSINATURA              PIC 9(003)V99    VALUE ZEROS.


       WORKING-STORAGE                         SECTION.

       01  FS-CLIENTES                 PIC X(002)          VALUE SPACES.
      *-----------------------------------------------------------------*

       01  WRK-QT-LINHAS               PIC 9(003) COMP-3   VALUE ZEROS.
       01  ACU-LIDOS                   PIC 9(003) COMP-3   VALUE ZEROS.
      *-----------------------------------------------------------------*

       01  WRK-MSG-ERRO                PIC X(030)          VALUE SPACES.
       01  WRK-CLIENTES                PIC X(008)          VALUE
               "CLIENTES".
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

               IF FS-CLIENTES                      NOT EQUAL '00'
                   MOVE WRK-ERRO-ABERTURA          TO WRK-DESCRICAO-ERRO
                   MOVE FS-CLIENTES                TO WRK-STATUS-ERRO
                   MOVE '0104-TESTAR-STATUS'       TO WRK-AREA-ERRO
                   PERFORM 9999-TRATA-ERRO
               END-IF.

       0104-TESTAR-STATUS-FIM.EXIT.

      *-----------------------------------------------------------------*
       0105-TESTAR-VAZIO                       SECTION.
      *-----------------------------------------------------------------*

               PERFORM 0106-LER-DADOS.

               IF FS-CLIENTES                  NOT EQUAL '00'
                   MOVE WRK-VAZIO              TO WRK-DESCRICAO-ERRO
                   MOVE FS-CLIENTES            TO WRK-STATUS-ERRO
                   MOVE '0105-TESTAR-VAZIO'    TO WRK-AREA-ERRO
                   PERFORM 9999-TRATA-ERRO
               ELSE
                   PERFORM 0210-IMP-CABECALHO
                   ADD 1 TO WRK-PAG
               END-IF.

       0105-TESTAR-VAZIO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0106-LER-DADOS                          SECTION.

               READ CLIENTES.

       0106-LER-DADOS-FIM.EXIT.
      *-----------------------------------------------------------------*
       0200-PROCESSAR                          SECTION.
      *-----------------------------------------------------------------*
               ADD 1 TO ACU-LIDOS.
               IF  WRK-QT-LINHAS GREATER 12
                   PERFORM 0210-IMP-CABECALHO
               END-IF.
                   PERFORM 0250-IMP-DETALHE
                   PERFORM 0106-LER-DADOS.


       0200-PROCESSAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0210-IMP-CABECALHO                          SECTION.
      *-----------------------------------------------------------------*
               DISPLAY  " ".
               DISPLAY WRK-CABEC1.
               MOVE ALL "=" TO WRK-CABEC3.
               DISPLAY WRK-CABEC3.
               DISPLAY WRK-CABEC2.

               MOVE ALL "=" TO WRK-CABEC3.
               DISPLAY WRK-CABEC3.
               MOVE 4 TO WRK-QT-LINHAS.


       0210-IMP-CABECALHO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0250-IMP-DETALHE                        SECTION.
      *-----------------------------------------------------------------*

               MOVE REG-CODIGO       TO WRK-DET-CODIGO.
               MOVE REG-NOME         TO WRK-DET-NOME.
               MOVE REG-EMAIL        TO WRK-DET-EMAIL.
               MOVE REG-STREAM       TO WRK-DET-STREAM.
               MOVE REG-ASSINATURA   TO WRK-DET-ASSIN.

               DISPLAY WRK-DETALHE.

               ADD 1 TO WRK-QT-LINHAS.

       0250-IMP-DETALHE-FIM.EXIT.
      *-----------------------------------------------------------------*
       0260-IMP-ESTATISTICA                    SECTION.
      *-----------------------------------------------------------------*

               DISPLAY "TOTAL DE REGISTROS LIDOS :" ACU-LIDOS.

       0260-IMP-ESTATISTICA-FIM.EXIT.
      *-----------------------------------------------------------------*
       0300-FINALIZAR                          SECTION.
      *-----------------------------------------------------------------*

               MOVE ALL "=" TO WRK-CABEC3.
               DISPLAY WRK-CABEC3.
               PERFORM 0260-IMP-ESTATISTICA.
               CLOSE CLIENTES.
               IF FS-CLIENTES                  NOT EQUAL '00'
                   MOVE WRK-ARQ-FECHADO        TO WRK-MSG-ERRO
                   MOVE FS-CLIENTES            TO WRK-STATUS-ERRO
                   MOVE WRK-CLIENTES           TO WRK-ARQUIVO-ERRO
               END-IF.
               GOBACK.

      *-----------------------------------------------------------------*
       0300-FINALIZAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       9000-MENSSAGEM-ERRO                     SECTION.

               DISPLAY WRK-MSG-ERRO.

       9000-MENSSAGEM-ERRO-FIM.EXIT.
      *-----------------------------------------------------------------*
       9999-TRATA-ERRO                         SECTION.
      *-----------------------------------------------------------------*

               DISPLAY "===== ERRO NO PROGRAMA ====="
               DISPLAY "MENSSAGEM....:"    WRK-DESCRICAO-ERRO.
               DISPLAY "FILE STATUS..:"    WRK-STATUS-ERRO.
               DISPLAY "AREA / SECAO.:"    WRK-AREA-ERRO.
               GOBACK.

       9999-TRATA-ERRO-FIM.EXIT.
      *-----------------------------------------------------------------*
