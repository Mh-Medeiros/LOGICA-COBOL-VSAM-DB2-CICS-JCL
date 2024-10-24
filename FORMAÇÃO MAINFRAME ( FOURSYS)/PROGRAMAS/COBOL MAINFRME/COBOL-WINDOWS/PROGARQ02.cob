       IDENTIFICATION                          DIVISION.
      *=================================================================
       PROGRAM-ID.PROGARQ02.
      *==== PROGRAMADOR.:MATHEUS
      *=================================================================
      *      EMPRESA... :  FOURSYS                                      *
      *=================================================================
      *    PROGRAMA.... :                                               *
      *    PROGRAMADOR. :                                               *
      *    DATA........ : 09 / 07 / 2024                                *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *-----------------------------------------------------------------
      *    OBJETIVO.... : LISTAR UM REGISTRO DO  ARQUIVO                *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *    OBSERVACOES. :                                               *
      *=================================================================
      *    ARQUIVOS.... :   CLIENTES.DAT.TXT                                            *
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
               "C:\CURSOLOGICA\COBOL\DADOS\CLIENTES.DAT.TXT"
               FILE STATUS IS FS-CLIENTES.

       DATA                                    DIVISION.
       FILE                                    SECTION.
       FD  CLIENTES.
       01  REG-CLIENTES.
           05 REG-ID                       PIC 9(04).
           05 REG-NOME                     PIC X(20).
           05 REG-TEL                      PIC X(11).

       WORKING-STORAGE                         SECTION.
       77  FS-CLIENTES                     PIC 9(02).

      *----------------- VARIAVEL DE MENSSAGEM
       01  WRK-MSG-ERRO.
           05 WRK-NAO-EXISTE               PIC X(30)        VALUE
                   "ARQUIVO NAO EXISTE!".
           05 WRK-ARQ-EXISTE               PIC X(30)        VALUE
                   "ARQUIVO ENCONTRADO!".
           05 WRK-VAZIO                    PIC x(30)        VALUE
                   "ARQUIVO VAZIO!".
       PROCEDURE                               DIVISION.
       0000-PRINCIPAL.
               PERFORM 0100-INICIAR.
               PERFORM 0200-PROCESSAR.
               PERFORM 0300-FINALIZAR.
               STOP RUN.
       0099-PRINCIPAL-FIM.EXIT.

       0100-INICIAR                            SECTION.
               OPEN INPUT CLIENTES.
               IF FS-CLIENTES EQUAL 35
                   DISPLAY WRK-NAO-EXISTE
               ELSE
                   DISPLAY WRK-ARQ-EXISTE
               END-IF.
               DISPLAY "STATUS... " FS-CLIENTES.
               DISPLAY "-------------------------".
       0199-INICIAR-FIM.EXIT.

       0200-PROCESSAR                          SECTION.
               READ CLIENTES
               IF FS-CLIENTES NOT EQUAL 10
                   DISPLAY "ID       :" REG-ID
                   DISPLAY "NOME     :" REG-NOME
                   DISPLAY "TELEFONE :" REG-TEL
              ELSE
                  DISPLAY WRK-VAZIO
              END-IF.
       0299-PROCESSAR-FIM.EXIT.

       0300-FINALIZAR                          SECTION.
           CLOSE CLIENTES.
           DISPLAY "FIM DO PROGRAMA".

       0399-FINALIZAR-FIM.EXIT.
