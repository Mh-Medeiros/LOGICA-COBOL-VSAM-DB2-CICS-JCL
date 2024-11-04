       IDENTIFICATION                          DIVISION.
      *=================================================================
       PROGRAM-ID.PROGRELCOMP.
      *
      *=================================================================
      *    EMPRESA... :  FOURSYS                                      *
      *=================================================================
      *    PROGRAMA.... :                                               *
      *    PROGRAMADOR.: MATHEUS                                       *
      *    DATA...... : 24/ 07 / 2024                                *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *-----------------------------------------------------------------
      *    OBJETIVO.... :  ABERTURA E LEITURA DOS REGISTROS DO ARQUIVO  *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *    OBSERVACOES. : FILTRAR RESGISTROS ESPECIFICOS NO ARQUIVO     *
      *=================================================================
      *    ARQUIVOS.... : LANCAM                            BOOK'S      *
      *                  RELSAIDA                        #BOOKLANCAM    *
      *                                                   #BOOKBANK     *
      *     TIPO....... : INPUT  / OUTPUT                               *
      *=================================================================
      *    MODULOS..... :                                               *
      *                                                                 *
      *=================================================================
       ENVIRONMENT                             DIVISION.
       CONFIGURATION                           SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT                            SECTION.
       FILE-CONTROL.
           SELECT LANCAM  ASSIGN TO
               "C:\CURSOLOGICA\COBOL\Atividade\LANCAM.TXT"
               FILE STATUS IS FS-LANCAM.
           SELECT RELSAIDA ASSIGN TO
               "C:\CURSOLOGICA\COBOL\Atividade\RELSAIDA.TXT"
               FILE STATUS IS FS-RELSAIDA.

       DATA                                    DIVISION.
       FILE                                    SECTION.
       FD  LANCAM.
       COPY "#BOOKLANCAM".

       FD  RELSAIDA.
       COPY "#BOOKRELSAIDA".

       WORKING-STORAGE                         SECTION.
      *=================================================================
       01  FILLER                      PIC X(50)           VALUE
             "========== VARIAVEL DE STATUS ========== ".
      *-----------------------------------------------------------------
       01  FS-LANCAM                   PIC X(02)           VALUE SPACES.
       01  FS-RELSAIDA                 PIC X(02)           VALUE SPACES.
      *=================================================================
       01  FILLER                      PIC X(50)           VALUE
               "========== VARIAVEIS ACUMULADORAS ========== ".
      *-----------------------------------------------------------------
       01  WRK-ACUM-LIDOS              PIC 9(03)    COMP-3 VALUE ZEROS.
       01  WRK-ACUM-VALIDOS            PIC 9(03)    COMP-3 VALUE ZEROS.
       01  WRK-ACUM-GRANA              PIC 9(10)V99 COMP-3 VALUE ZEROS.

      *=================================================================
       01  FILLER                      PIC X(50)           VALUE
               "========== VARIAVEL DE APOIO ==========".
      *-----------------------------------------------------------------
       01  WRK-PASSOU                  PIC X(01)           VALUE SPACES.
       01  WRK-MSG-ERROS               PIC X(30)           VALUE SPACES.
       01  WRK-FILE-STATUS             PIC 9(02)           VALUE ZEROS.
       01  WRK-ARQUIVO                 PIC X(10)           VALUE SPACES.

      *=================================================================
       01  FILLER                      PIC x(50)           VALUE
               "===== VARIAVEIS DE EDICAO (MASCARAS) =====".
      *-----------------------------------------------------------------
       01  WRK-LANCAMENTO-ED           PIC Z.ZZZ.ZZ9,99.
      *=================================================================
      *=================================================================
       01  FILLER                      PIC x(50)           VALUE
               "========== BOOK DE MENSSAGENS ==========".
      *-----------------------------------------------------------------
       COPY "#BOOKBANK".
      *=================================================================

       PROCEDURE                               DIVISION.
       0000-PRINCIPAL.

               PERFORM 0100-INICIAR.
               PERFORM 0200-PROCESSAR UNTIL FS-LANCAM NOT EQUAL '00'.
               PERFORM 0300-FINALIZAR.
               STOP RUN.

       0000-PRINCIPAL-FIM.EXIT.
      *=================================================================
       0100-INICIAR                            SECTION.

               OPEN INPUT  LANCAM
                    OUTPUT RELSAIDA.
               PERFORM 0105-TESTAR-STATUS.
               PERFORM 0110-LEITURA.

       0100-INICIAR-FIM.EXIT.
       0105-TESTAR-STATUS                      SECTION.

       0106-TESTAR-STATUS-LANCAM.

               IF FS-LANCAM   NOT EQUAL '00'
                   MOVE WRK-NAO-ACHOU        TO WRK-MSG-ERROS
                   MOVE FS-LANCAM            TO WRK-FILE-STATUS
                   MOVE WRK-LANCAM           TO WRK-ARQUIVO
                   PERFORM 9999-TRATA-ERRO
               END-IF.

       0107-TESATAR-STATUS-RELSAIDA.

               IF FS-RELSAIDA   NOT EQUAL '00'
                   MOVE WRK-NAO-ACHOU        TO WRK-MSG-ERROS
                   MOVE FS-LANCAM            TO WRK-FILE-STATUS
                   MOVE WRK-RELSAIDA         TO WRK-ARQUIVO

                   PERFORM 9999-TRATA-ERRO
                   PERFORM 0310-FINALIZAR-LANCAM
               END-IF.

       0105-TESTAR-STATUS-FIM.EXIT.

      *=================================================================
       0110-LEITURA                            SECTION.

               READ LANCAM
               IF FS-LANCAM EQUAL '00' OR FS-LANCAM EQUAL '10'
                 CONTINUE
               ELSE
                   MOVE WRK-ERRO-LEITURA TO  WRK-MSG-ERROS
               END-IF.

       0110-LEITURA-FIM.EXIT.

       0200-PROCESSAR                          SECTION.

               ADD 1 TO WRK-ACUM-LIDOS.
               PERFORM 0220-VALIDA-REG.

               IF WRK-PASSOU  EQUAL 'S'
                   ADD 1 TO WRK-ACUM-VALIDOS
                       MOVE REG-AGENCIA    TO REG-AGENCIA-RELSAIDA
                       MOVE REG-CONTA      TO REG-CONTA-RELSAIDA
                       MOVE REG-LANCAMENTO TO REG-LANCAMENTO-RELSAIDA
                       WRITE REG-RELSAIDA

                       IF FS-RELSAIDA NOT EQUAL '00'
                           MOVE WRK-NAO-GRAVOU TO WRK-MSG-ERROS
                           PERFORM 9999-TRATA-ERRO
                       ELSE
                           PERFORM 0210-ACUMULA
                       END-IF
               END-IF.
               PERFORM 0110-LEITURA.
               IF FS-LANCAM NOT EQUAL '00'
                   PERFORM 0230-ESTATISTICA
               END-IF.

       0200-PROCESSAR-FIM.EXIT.
      *=================================================================
       0210-ACUMULA                            SECTION.

               ADD REG-LANCAMENTO TO WRK-ACUM-GRANA.
               MOVE WRK-ACUM-GRANA TO WRK-LANCAMENTO-ED.

       0210-ACUMULA-FIM.EXIT.
      *=================================================================
       0220-VALIDA-REG                         SECTION.

               IF REG-GERENTE   EQUAL 'P' AND REG-TIPO-CLI  EQUAL 'F'
                   MOVE 'S' TO WRK-PASSOU
               ELSE
                   MOVE 'N' TO WRK-PASSOU
               END-IF.


       0220-VALIDA-REG-FIM.EXIT.
      *=================================================================
       0230-ESTATISTICA                        SECTION.

               DISPLAY " >>>>>>>> REGISTROS <<<<<<<<< "
               DISPLAY "TOTAL DE REGISTROS LIDOS....:" WRK-ACUM-LIDOS.
               DISPLAY "TOTAL DE REGISTROS VALIDOS..:" WRK-ACUM-VALIDOS.
               DISPLAY "TOTAL DE LANCAMENTOS........:R$"
                   WRK-LANCAMENTO-ED.

       0230-ESTATISTICA-FIM.EXIT.
      *=================================================================
       0300-FINALIZAR                          SECTION.

       0310-FINALIZAR-LANCAM.
               CLOSE LANCAM
                     RELSAIDA.

               IF FS-LANCAM            NOT EQUAL '00'
                   MOVE WRK-ARQ-OK     TO WRK-MSG-ERROS
                   MOVE FS-LANCAM      TO WRK-FILE-STATUS
                   MOVE WRK-LANCAM     TO WRK-ARQUIVO
                   PERFORM 9999-TRATA-ERRO
                   GOBACK
               END-IF.


       0320-FINALIZAR-RELSAIDA.


               IF FS-LANCAM            NOT EQUAL '00'
                   MOVE WRK-ARQ-OK       TO WRK-MSG-ERROS
                   MOVE FS-LANCAM        TO WRK-FILE-STATUS
                   MOVE WRK-RELSAIDA     TO WRK-ARQUIVO

                   PERFORM 9999-TRATA-ERRO
                   GOBACK
               END-IF.

       0300-FINALIZAR-FIM.EXIT.
      *=================================================================
       0350-MENSSAGENS                         SECTION.

               DISPLAY  WRK-MSG-ERROS.

       0350-MENSSAGENS-FIM.EXIT.




      *=================================================================
       9999-TRATA-ERRO                          SECTION.

               DISPLAY "========== PROGRAMA INTERROMPIDO =========".
               DISPLAY "STATUS......:" WRK-FILE-STATUS.
               DISPLAY "ARQUIVO.....:" WRK-ARQUIVO.
               DISPLAY "STATUS ERRO.:" WRK-MSG-ERROS.
               GOBACK.

       9999-TRATA-ERRO-FIM.EXIT.
