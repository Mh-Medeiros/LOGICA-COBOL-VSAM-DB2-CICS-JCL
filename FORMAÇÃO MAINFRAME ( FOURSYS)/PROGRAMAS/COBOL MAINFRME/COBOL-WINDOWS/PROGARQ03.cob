       IDENTIFICATION                          DIVISION.
      *=================================================================
       PROGRAM-ID.PROGARQ03.
      *==== PROGRAMADOR.:MATHEUS
      *=================================================================
      *      EMPRESA... :  FOURSYS                                      *
      *=================================================================
      *    DATA........ : 11 / 07 / 2024                                *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *-----------------------------------------------------------------
      *    OBJETIVO.... : LEITURA DE TODOS OS REGISTROS DO  ARQUIVO     *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *    OBSERVACOES. :                                               *
      *=================================================================
      *    ARQUIVOS.... : CLIENTES.DAT.TXT                              *
      *                                                     BOOK        *
      *                                                     ----        *
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
           SELECT CLIENTES ASSIGN TO
               "C:\CURSOLOGICA\COBOL\DADOS\NEWCLI.DAT.TXT"
                   FILE STATUS IS FS-CLIENTES.
       DATA                                    DIVISION.
       FILE                                    SECTION.
       FD  CLIENTES.
       01  REG-CLIENTES.
           05 REG-ID                   PIC 9(04).
           05 REG-NOME                 PIC X(20).
           05 REG-TEL                  PIC X(11).

       WORKING-STORAGE                         SECTION.
       77  FS-CLIENTES                 PIC 9(02)           VALUE ZEROS.

      *----------------- VARIAVEL DE MENSSAGEM
       01  WRK-MSG-ERRO.
           05 WRK-MSG-NAO-ACHOU        PIC X(30)           VALUE
               "ARQUIVO NAO ENCONTRADO".
           05 WRK-MSG-ARQ-OK           PIC X(30)           VALUE
               "ARQUIVO ENCONTRADO".
           05 WRK-VAZIO                PIC X(30)           VALUE
               "ARQUIVO VAZIO!".

       PROCEDURE                               DIVISION.
       0000-PRINCIPAL.
               PERFORM 0100-INICIAR.
               PERFORM 0200-PROCESSAR UNTIL FS-CLIENTES NOT EQUAL 0
               PERFORM 0300-FINALIZAR.
               STOP RUN.

       0099-PRINCIPAL-FIM.EXIT.

       0100-INICIAR                            SECTION.
               OPEN INPUT CLIENTES.
               IF FS-CLIENTES NOT EQUAL 0
                   DISPLAY "STATUS... " FS-CLIENTES
                   DISPLAY WRK-MSG-NAO-ACHOU
                   DISPLAY "------------------------"
               END-IF.


       0199-INICIAR-FIM.EXIT.

       0200-PROCESSAR                          SECTION.

              READ CLIENTES
              IF FS-CLIENTES EQUAL 0
                  PERFORM UNTIL FS-CLIENTES NOT EQUAL 00
                       DISPLAY "ID      :" REG-ID
                       DISPLAY "NOME    :" REG-NOME
                       DISPLAY "TELEFONE:" REG-TEL
                       DISPLAY "---------------------------"
                 READ CLIENTES
                 END-PERFORM
              ELSE
                   DISPLAY WRK-VAZIO
              END-IF.

       0299-PROCESSAR-FIM.EXIT.

       0300-FINALIZAR                          SECTION.
               CLOSE CLIENTES.
               DISPLAY "FIM DO PROGRAMA".

       0399-FINALIZAR-FIM.EXIT.
