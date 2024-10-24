       IDENTIFICATION                          DIVISION.
      *=================================================================
       PROGRAM-ID.                             PROGCALL.
      *=================================================================
      * PROGRAMA   : PROGCALL
      * PROGRAMADOR: MATHEUS H. MEDEIROS
      * ANALISTA   : IVAN SANCHES
      * CONSULTORIA: FOURSYS
      * DATA.......: 22/07/2024
      *-----------------------------------------------------------------
      * OBJETIVO...: LER ARQUIVO DE LANCAMENTOS E MOSTRAR VALIDOS
      *              E GRAVACAO DE DADOS EM ARQUIVO
      *-----------------------------------------------------------------
      * ARQUIVOS                I/O                  INCLUDE/BOOK
      *  LANCAM                 I                    #BOOKLANCAM
      *  RELSAIDA               O                   #BOOKRELSAIDA
      *
      *-----------------------------------------------------------------
      * MODULOS....: LOGS
      *
      *-----------------------------------------------------------------
      *                          ALTERACOES                             *
      *-----------------------------------------------------------------
      * PROGRAMADOR:
      * ANALISTA   :
      * CONSULTORIA:
      * DATA.......:
      * OBJETIVO...:
      *
      *=================================================================


      *=================================================================
       ENVIRONMENT                             DIVISION.
      *=================================================================

      *-----------------------------------------------------------------
       CONFIGURATION                           SECTION.
      *-----------------------------------------------------------------

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       INPUT-OUTPUT                            SECTION.
      *-----------------------------------------------------------------

       FILE-CONTROL.
           SELECT LANCAM  ASSIGN TO
               "C:\CURSOLOGICA\COBOL\Atividade\LANCAM.TXT"
               FILE STATUS IS FS-LANCAM.
           SELECT RELSAIDA ASSIGN TO
               "C:\CURSOLOGICA\COBOL\Atividade\RELSAIDA.TXT"
               FILE STATUS IS FS-RELSAIDA.

      *=================================================================
       DATA                                    DIVISION.
      *=================================================================

      *-----------------------------------------------------------------
       FILE                                    SECTION.
      *-----------------------------------------------------------------
      *        INPUT -  DADOS DO ARQUIVO DE ENTRADA (LANCAM)
      *                               LRECL = 021
      *
      *-----------------------------------------------------------------
       FD  LANCAM.
       COPY "#BOOKLANCAM".
      *-----------------------------------------------------------------
      *     OUTPUT-  DADOS DO ARQUIVO DE SAIDA (REGCAM)
      *                               LRECL = 019
      *-----------------------------------------------------------------
       FD  RELSAIDA.
       COPY "#BOOKRELSAIDA".

      *-----------------------------------------------------------------
       WORKING-STORAGE                         SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       01  FILLER                      PIC X(050)          VALUE
           "* INICIO DA WORKING *".
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       01  FILLER                      PIC X(050)           VALUE
             "========== VARIAVEL DE STATUS ========== ".
      *-----------------------------------------------------------------
       01  FS-LANCAM                   PIC X(002)          VALUE SPACES.
       01  FS-RELSAIDA                 PIC X(002)          VALUE SPACES.
      *-----------------------------------------------------------------
       01  FILLER                      PIC X(050)           VALUE
               "========== VARIAVEIS ACUMULADORAS ========== ".
      *-----------------------------------------------------------------
       01  ACU-LIDOS-LANCAM            PIC 9(003)    COMP-3 VALUE ZEROS.
       01  ACU-VALIDOS-LANCAM          PIC 9(003)    COMP-3 VALUE ZEROS.
       01  ACU-GRANA-LANCAM            PIC 9(010)V99 COMP-3 VALUE ZEROS.
       01  ACU-GRAVA-RELSAIDA          PIC 9(003)    COMP-3 VALUE ZEROS.
      *-----------------------------------------------------------------
       01  FILLER                      PIC X(050)           VALUE
               "========== VARIAVEL DE APOIO ==========".
      *-----------------------------------------------------------------
       01  WRK-PASSOU                  PIC X(001)          VALUE SPACES.
       01  WRK-FILE-STATUS             PIC 9(002)          VALUE ZEROS.
       01  WRK-ARQUIVO                 PIC X(010)          VALUE SPACES.
       01  WRK-MODULO                  PIC X(008)          VALUE 'LOGS'.
       01  WRK-LANCAM                  PIC X(10)           VALUE
                "LANCAM".
       01  WRK-RELSAIDA                PIC x(10)           VALUE
                "RELSAIDA".
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       01  FILLER                      PIC x(050)           VALUE
               "===== VARIAVEIS DE EDICAO (MASCARAS) =====".
      *-----------------------------------------------------------------
       01  WRK-LANCAMENTO-ED           PIC Z.ZZZ.ZZ9,99.
      *-----------------------------------------------------------------
       01  FILLER                      PIC x(050)           VALUE
               "========== BOOK'S  ==========".
      *-----------------------------------------------------------------
       COPY "#BOOKBANK".
       COPY "#BOOKERRO".
      *-----------------------------------------------------------------
       01  FILLER                      PIC X(050)          VALUE
               "* FIM DA WORKING *".
      *-----------------------------------------------------------------
      *=================================================================
       PROCEDURE                               DIVISION.
      *=================================================================
       0000-PRINCIPAL.

               PERFORM 0100-INICIAR.
               PERFORM 0200-PROCESSAR UNTIL FS-LANCAM NOT EQUAL '00'.
               PERFORM 0300-FINALIZAR.
               STOP RUN.

       0000-PRINCIPAL-FIM.                     EXIT.
      *-----------------------------------------------------------------
       0100-INICIAR                            SECTION.
      *-----------------------------------------------------------------

               OPEN INPUT  LANCAM
                    OUTPUT RELSAIDA.

               PERFORM 0105-TESTAR-STATUS.
               PERFORM 0110-LEITURA.

       0100-INICIAR-FIM.                       EXIT.
      *-----------------------------------------------------------------
       0105-TESTAR-STATUS                      SECTION.
      *-----------------------------------------------------------------
       0106-TESTAR-STATUS-LANCAM.

               IF FS-LANCAM                  NOT EQUAL '00'
                   MOVE WRK-ERRO-ABERTURA    TO WRK-DESCRICAO-ERRO
                   MOVE FS-LANCAM            TO WRK-STATUS-ERRO
                   MOVE '0106-TESTE-LANCAM'  TO WRK-AREA-ERRO
                   PERFORM 9999-TRATA-ERRO
               END-IF.

       0107-TESTAR-STATUS-RELSAIDA.

               IF FS-RELSAIDA                     NOT EQUAL '00'
                   MOVE WRK-NAO-ACHOU             TO WRK-DESCRICAO-ERRO
                   MOVE FS-LANCAM                 TO WRK-STATUS-ERRO
                   MOVE '0106-TESTE-LANCAM'       TO WRK-AREA-ERRO
                   PERFORM 0310-FINALIZAR-LANCAM
                   PERFORM 9999-TRATA-ERRO
               END-IF.

       0105-TESTAR-STATUS-FIM.                 EXIT.
      *-----------------------------------------------------------------
       0110-LEITURA                            SECTION.
      *-----------------------------------------------------------------

               READ LANCAM
               IF FS-LANCAM EQUAL '00' OR FS-LANCAM EQUAL '10'
                 CONTINUE
               ELSE
                   MOVE WRK-ERRO-LEITURA   TO  WRK-DESCRICAO-ERRO
                   MOVE FS-LANCAM          TO  WRK-STATUS-ERRO
                   MOVE '0110-LEITURA'     TO  WRK-AREA-ERRO
                   PERFORM 0350-MENSSAGENS
                   GOBACK
               END-IF.

       0110-LEITURA-FIM.                       EXIT.
      *-----------------------------------------------------------------
       0200-PROCESSAR                          SECTION.
      *-----------------------------------------------------------------

               ADD 1 TO ACU-LIDOS-LANCAM.
               PERFORM 0220-VALIDA-REG.

               IF WRK-PASSOU                 EQUAL 'S'
                   ADD 1 TO ACU-VALIDOS-LANCAM
                       MOVE REG-AGENCIA      TO REG-AGENCIA-RELSAIDA
                       MOVE REG-CONTA        TO REG-CONTA-RELSAIDA
                       MOVE REG-LANCAMENTO   TO REG-LANCAMENTO-RELSAIDA

                       PERFORM 0240-GRAVA
               END-IF.
               PERFORM 0110-LEITURA.
               IF FS-LANCAM                  NOT EQUAL '00'
                   PERFORM 0230-ESTATISTICA
               END-IF.

       0200-PROCESSAR-FIM.                     EXIT.
      *-----------------------------------------------------------------
       0210-ACUMULA                            SECTION.
      *-----------------------------------------------------------------

               ADD  REG-LANCAMENTO  TO ACU-GRANA-LANCAM.
               MOVE ACU-GRANA-LANCAM  TO WRK-LANCAMENTO-ED.

       0210-ACUMULA-FIM.                       EXIT.

      *-----------------------------------------------------------------
       0220-VALIDA-REG                         SECTION.
      *-----------------------------------------------------------------

               IF REG-GERENTE   EQUAL 'P' AND REG-TIPO-CLI  EQUAL 'F'
                   MOVE 'S' TO WRK-PASSOU
               ELSE
                   MOVE 'N' TO WRK-PASSOU
               END-IF.

       0220-VALIDA-REG-FIM.                    EXIT.
      *-----------------------------------------------------------------
       0230-ESTATISTICA                        SECTION.
      *-----------------------------------------------------------------

               DISPLAY " >>>>>>>> REGISTROS <<<<<<<<< "
               DISPLAY "TOTAL DE REGISTROS LIDOS....:"
                   ACU-LIDOS-LANCAM.
               DISPLAY "TOTAL DE REGISTROS VALIDOS..:"
                   ACU-VALIDOS-LANCAM.
               DISPLAY "TOTAL DE REGISTROS GRAVADOS.:"
                   ACU-GRAVA-RELSAIDA.
               DISPLAY "TOTAL DE LANCAMENTOS.....:R$"
                   WRK-LANCAMENTO-ED.

       0230-ESTATISTICA-FIM.                   EXIT.
      *-----------------------------------------------------------------
       0240-GRAVA                              SECTION.
      *-----------------------------------------------------------------

               WRITE REG-RELSAIDA.

               IF FS-RELSAIDA             NOT EQUAL '00'
                   MOVE WRK-NAO-GRAVOU    TO WRK-MSG-ERROS
                   MOVE FS-RELSAIDA       TO WRK-STATUS-ERRO
                   MOVE "0240-GRAVA-REG"  TO WRK-AREA-ERRO

                   PERFORM 9999-TRATA-ERRO
               ELSE
                  PERFORM 0210-ACUMULA
                  ADD 1 TO ACU-GRAVA-RELSAIDA
               END-IF.

       0240-GRAVA-FIM.                     EXIT.



      *-----------------------------------------------------------------
       0300-FINALIZAR                          SECTION.
      *-----------------------------------------------------------------

       0310-FINALIZAR-LANCAM.
               CLOSE LANCAM
                     RELSAIDA.
               IF FS-LANCAM                NOT EQUAL '00'
                   MOVE WRK-ARQ-OK         TO WRK-MSG-ERROS
                   MOVE FS-LANCAM          TO WRK-STATUS-ERRO
                   MOVE WRK-LANCAM         TO WRK-ARQUIVO
                   PERFORM 9999-TRATA-ERRO
                   GOBACK
               END-IF.

       0320-FINALIZAR-RELSAIDA.

               IF FS-LANCAM                NOT EQUAL '00'
                   MOVE WRK-ARQ-OK         TO WRK-MSG-ERROS
                   MOVE FS-LANCAM          TO WRK-FILE-STATUS
                   MOVE WRK-RELSAIDA       TO WRK-ARQUIVO

                   PERFORM 9999-TRATA-ERRO
                   GOBACK
               END-IF.

       0300-FINALIZAR-FIM.                     EXIT.

      *-----------------------------------------------------------------
       0350-MENSSAGENS                         SECTION.
      *-----------------------------------------------------------------

               DISPLAY "===== ERRO NO PROGRAMA ====="
               DISPLAY "ARQUIVO......:"    WRK-PROGRAMA-ERRO.
               DISPLAY "MENSSAGEM....:"    WRK-DESCRICAO-ERRO.
               DISPLAY "FILE STATUS..:"    WRK-STATUS-ERRO.
               DISPLAY "AREA / SECAO.:"    WRK-AREA-ERRO.

       0350-MENSSAGENS-FIM.                     EXIT.


      *-----------------------------------------------------------------
       9999-TRATA-ERRO                          SECTION.
      *-----------------------------------------------------------------

      *         DISPLAY WRK-MSG-ERROS.
                CALL WRK-MODULO USING WRK-MSG-ERROS.
                GOBACK.


      *-----------------------------------------------------------------
       9999-TRATA-ERRO-FIM.                      EXIT.
      *-----------------------------------------------------------------
