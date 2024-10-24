       IDENTIFICATION                          DIVISION.
      *=================================================================
       PROGRAM-ID.PROGARQ05.
      *==== PROGRAMADOR.:MATHEUS
      *=================================================================
      *      EMPRESA... :  FOURSYS                                             *
      *=================================================================
      *    PROGRAMA.... :                                               *
      *    PROGRAMADOR. :                                               *
      *    DATA........ : 12 / 07 / 2024                                            *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *-----------------------------------------------------------------
      *    OBJETIVO.... :  GRAVACAO DE REGISTRO NO  ARQUIVO                                              *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *    OBSERVACOES. :                                               *
      *=================================================================
      *    ARQUIVOS.... : NEWCLI.DAT.TXT                                            *
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
       77  WRK-GRAVA                   PIC X(01)           VALUE "S".
       77  WRK-ERRO                    PIC X(30)           VALUE SPACES.
      *----------------- VARIAVEL DE MENSSAGEM
       01  WRK-MSG-ERRO.
           05 WRK-MSG-NAO-ACHOU        PIC X(30)           VALUE
               "ARQUIVO NAO ENCONTRADO".
           05 WRK-MSG-ARQ-OK           PIC X(30)           VALUE
               "ARQUIVO ENCONTRADO".
           05 WRK-REG-OK               PIC X(30)           VALUE
               "REGISTRADO(S)!!".
           05 WRK-FIM-ARQ              PIC X(30)           VALUE
               "FIM DE PROGRAMA".
       PROCEDURE                               DIVISION.
       0000-PRINCIPAL.
               PERFORM 0100-INICIAR.
               PERFORM 0200-PROCESSAR
               PERFORM 0300-FINALIZAR
               STOP RUN.
       0000-PRINCIPAL-FIM.EXIT.

       0100-INICIAR                            SECTION.

               OPEN EXTEND CLIENTES.
                   IF FS-CLIENTES EQUAL 35
                       DISPLAY  WRK-MSG-NAO-ACHOU
                       GOBACK
                   ELSE
                       DISPLAY WRK-MSG-ARQ-OK
               END-IF.

       0100-INICIAR-FIM.EXIT.

       0200-PROCESSAR                          SECTION.
               PERFORM UNTIL WRK-GRAVA NOT EQUAL "S"
                   DISPLAY "ID: "
                       ACCEPT REG-ID
                   DISPLAY "NOME: "
                       ACCEPT REG-NOME
                   DISPLAY "TEL :"
                       ACCEPT REG-TEL
                   WRITE REG-CLIENTES

                        DISPLAY "DESEJA CONTINUAR?"
                    DISPLAY "[S]IM / [N]AO"
                       ACCEPT WRK-GRAVA
                    DISPLAY  WRK-REG-OK

                END-PERFORM.

       0200-PROCESSAR-FIM.EXIT.

       0300-FINALIZAR                          SECTION.

               CLOSE CLIENTES
               MOVE WRK-FIM-ARQ  TO WRK-ERRO
               PERFORM 9000-TRATA-ERRO.


       0300-FINALIZAR-FIM.EXIT.

       9000-TRATA-ERRO                         SECTION.
               DISPLAY WRK-ERRO.
               GOBACK.
       9000-TRATA-ERRO-FIM.EXIT.
