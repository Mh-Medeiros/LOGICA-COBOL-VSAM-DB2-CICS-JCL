       IDENTIFICATION                          DIVISION.
       PROGRAM-ID.                             RVL4.
      *=================================================================*
      * PROGRAMA   : RVL4
      * PROGRAMADOR: MATHEUS MEDEIROS
      * ANALISTA   : IVAN SANCHES
      * CONSULTORIA: FOURSYS
      * DATA.......: 07 / 08 / 2024
      *-----------------------------------------------------------------*
      *    OBJETIVO...:   IMPRIMIR UM RELATORIO DOS ASSINANTE
      *    CALSSIFICADOS POR  STREAMING
      *-----------------------------------------------------------------*
      *    ARQUIVOS                I/O                  INCLUDE/BOOK
      *    CLIENTES               INPUT
      *    RELAT                  OUTPUT
      *
      *
      *
      *-----------------------------------------------------------------*
      * MODULOS....:
      *
      *-----------------------------------------------------------------*
      *=================================================================*
       ENVIRONMENT                             DIVISION.
      *=================================================================*


      *-----------------------------------------------------------------*
      *CONFIGURATION                           SECTION.
      *-----------------------------------------------------------------*

      * SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------*
       INPUT-OUTPUT                            SECTION.
      *-----------------------------------------------------------------*
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO
               "C:\CURSOLOGICA\COBOL\RELATORIOS\CLIENTES.DAT".

           SELECT SAIDA   ASSIGN TO
               "C:\CURSOLOGICA\COBOL\RELATORIOS\CLIENTES2.TXT".
                   SELECT WORK ASSIGN TO "WRK".
      *=================================================================*
       DATA                                    DIVISION.
      *=================================================================*
       FILE                                    SECTION.
      *-----------------------------------------------------------------*
      *        INPUT -  DADOS DO ARQUIVO DE SAIDA(CLIENTES)
      *                               LRECL = 58
      *
      *-----------------------------------------------------------------*
       FD  CLIENTES.
       01  REG-CLIENTES.
           05 REG-CLIENTES-P1             PIC X(043)       VALUE SPACES.
           05 REG-CLIENTES-CHAVE          PIC X(010)       VALUE SPACES.
           05 REG-CLIENTES-P2             PIC X(005)       VALUE SPACES.
      *-----------------------------------------------------------------*
       FD  SAIDA.
       01  REG-SAIDA.
           05 REG-SAIDA-P1                PIC X(043)       VALUE SPACES.
           05 REG-SAIDA-CHAVE             PIC X(010)       VALUE SPACES.
           05 REG-SAIDA-P2                PIC X(005)       VALUE SPACES.

      *-----------------------------------------------------------------*
       SD  WORK.
       01  REG-WORK.
           05 REG-WORK-P1                 PIC X(043)       VALUE SPACES.
           05 REG-WORK-CHAVE              PIC X(010)       VALUE SPACES.
           05 REG-WORK-P2                 PIC X(005)       VALUE SPACES.


       WORKING-STORAGE                         SECTION.
      *-----------------------------------------------------------------*

      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               "***** INICIO DA WORKING *****".
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " VARIAVEL DE STATUS ".
      *-----------------------------------------------------------------*
       01  FS-CLIENTES                  PIC 9(002)         VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  FILLER                       PIC X(050)         VALUE
               " VARIAVEL DE APOIO ".
      *-----------------------------------------------------------------*
       01  WRK-MSG-ERRO                 PIC X(030)         VALUE SPACES.

      *-----------------------------------------------------------------*
       01  FILLER                       PIC X(050)         VALUE
           "***** FIM DA WORKING *****".
      *-----------------------------------------------------------------*

      *=================================================================*
       PROCEDURE                               DIVISION.
           SORT WORK ON ASCENDING KEY REG-CLIENTES-CHAVE
           USING CLIENTES GIVING SAIDA.
           DISPLAY "SORT SUCCESSFUL".
       STOP RUN.
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

           OPEN INPUT  CLIENTES.


       0100-INICIAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0110-TESTAR-VAZIO                       SECTION.
      *-----------------------------------------------------------------*

           PERFORM 0120-LER-DADOS.
           IF FS-CLIENTES                  EQUAL  00
               CONTINUE
           END-IF.

       0110-TESTAR-VAZIO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0120-LER-DADOS                          SECTION.

           READ CLIENTES.

       0120-LER-DADOS-FIM.EXIT.
      *-----------------------------------------------------------------*
       0200-PROCESSAR                          SECTION.
      *-----------------------------------------------------------------
               CONTINUE.
       0200-PROCESSAR-FIM.EXIT.
      *-----------------------------------------------------------------*

      *-----------------------------------------------------------------*
       0300-FINALIZAR                          SECTION.
               CLOSE CLIENTES.
      *-----------------------------------------------------------------*
       0300-FINALIZAR-FIM.EXIT.
