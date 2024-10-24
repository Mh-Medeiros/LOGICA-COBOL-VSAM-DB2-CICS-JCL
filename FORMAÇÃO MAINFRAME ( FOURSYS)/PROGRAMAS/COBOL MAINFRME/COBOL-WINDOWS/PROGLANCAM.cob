       IDENTIFICATION                          DIVISION.
      *=================================================================
       PROGRAM-ID.PROGLANCAM.
      *
      *=================================================================
      *    EMPRESA... :  FOURSYS                                      *
      *=================================================================
      *    PROGRAMA....: PROGLANCAM                                              *
      *    PROGRAMADOR.: MATHEUS                                       *
      *    DATA....... : 18 / 07 / 2024                                *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *-----------------------------------------------------------------
      *    OBJETIVO.... : ABERTURA E LEITURA DOS REGISTROS DOO ARQUIVO  *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *    OBSERVACOES. : FILTRAR RESGISTROS ESPECIFICOS NO ARQUIVO     *
      *=================================================================
      *    ARQUIVOS.... : LANCAM                            BOOK'S      *
      *                                                  #BOOKLANCAM    *
      *                                                   #BOOKBANK     *
      *     TIPO....... : INPUT                                         *
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
           SELECT LANCAM ASSIGN TO
               "C:\CURSOLOGICA\COBOL\Atividade\LANCAM.TXT"
               FILE STATUS IS FS-LANCAM.

       DATA                                    DIVISION.
       FILE                                    SECTION.
       FD  LANCAM.
       COPY "#BOOKLANCAM".

       WORKING-STORAGE                         SECTION.
      *---------------- VARIAVEL DE STATUS
       77  FS-LANCAM                   PIC X(02)           VALUE ZEROS.
      *---------------- VARIAVEL DE ACUMULO
       77  WRK-ACUM-LIDOS              PIC 9(03)           VALUE ZEROS.
       77  WRK-ACUM-VALIDOS            PIC 9(03)           VALUE ZEROS.
      *---------------- VARIAVEL DE APOIO
       77  WRK-PASSOU                  PIC X(01)           VALUE SPACES.
       77  WRK-MSG-ERROS               PIC X(30)           VALUE SPACES.
      *---------------- MASCARAS
       77  WRK-LANCAMENTO-ED           PIC Z.ZZZ.ZZ9,99.

      *---------------- BOOKS
       COPY "#BOOKBANK".

       PROCEDURE                               DIVISION.
       0000-PRINCIPAL.

               PERFORM 0100-INICIAR.
               PERFORM 0200-PROCESSAR UNTIL FS-LANCAM NOT EQUAL 00.
               PERFORM 0300-FINALIZAR.
               GOBACK.

       0000-PRINCIPAL-FIM.EXIT.
      *=================================================================
       0100-INICIAR                            SECTION.

               OPEN INPUT LANCAM.
               DISPLAY "STATUS: "FS-LANCAM.
               IF FS-LANCAM   EQUAL 00
                   MOVE WRK-ARQ-OK  TO WRK-MSG-ERROS
                   PERFORM 0310-MENSSAGENS
                   PERFORM 0110-LEITURA
               ELSE
                   MOVE WRK-ERRO-ABERTURA TO WRK-MSG-ERROS
                   PERFORM 0310-MENSSAGENS
                   GOBACK
               END-IF.

       0100-INICIAR-FIM.EXIT.
      *=================================================================
       0110-LEITURA                            SECTION.

               READ LANCAM.

       0110-LEITURA-FIM.EXIT.

       0200-PROCESSAR                          SECTION.

               ADD 1 TO WRK-ACUM-LIDOS.
               MOVE REG-LANCAMENTO TO WRK-LANCAMENTO-ED
               PERFORM 0210-VALIDA-REG.
                 IF WRK-PASSOU  EQUAL 'S'
                      DISPLAY  "AGENCIA.........:"   REG-AGENCIA
                      DISPLAY  "CONTA...........:"   REG-CONTA
                      DISPLAY  "LANCAMENTOS.....: R$"WRK-LANCAMENTO-ED
                      DISPLAY  "GERENTE.........:"   REG-GERENTE
                      DISPLAY  "TIPO DE CLIENTE.:"   REG-TIPO-CLI
                      MOVE  WRK-SEPARALINHA     TO   WRK-MSG-ERROS
                      PERFORM 0310-MENSSAGENS
                      ADD 1 TO WRK-ACUM-VALIDOS
                 END-IF.
               PERFORM 0110-LEITURA.


       0200-PROCESSAR-FIM.EXIT.
      *=================================================================
       0210-VALIDA-REG                         SECTION.

               IF REG-GERENTE   EQUAL 'P' AND REG-TIPO-CLI  EQUAL 'F'
                   MOVE 'S' TO WRK-PASSOU
               ELSE
                   MOVE 'N' TO WRK-PASSOU
               END-IF.


       0210-VALIDA-REG-FIM.EXIT.
      *=================================================================
       0300-FINALIZAR                          SECTION.

               MOVE  WRK-SEPARALINHA    TO WRK-MSG-ERROS.
               PERFORM 0310-MENSSAGENS.
               DISPLAY "REGISTROS :" WRK-ACUM-LIDOS.
               DISPLAY "VALIDOS   :" WRK-ACUM-VALIDOS.
               CLOSE LANCAM.
               IF FS-LANCAM EQUAL 0
                   MOVE WRK-ERRO-FECHAR TO WRK-MSG-ERROS
                   PERFORM 0310-MENSSAGENS
               ELSE
                   MOVE WRK-FECHADO     TO WRK-MSG-ERROS
                   PERFORM 0310-MENSSAGENS.

       0300-FINALIZAR-FIM.EXIT.
      *=================================================================
       0310-MENSSAGENS                         SECTION.

               DISPLAY WRK-MSG-ERROS.

       0310-MENSSAGENS-FIM.EXIT.
