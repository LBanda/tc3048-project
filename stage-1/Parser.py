from Lexer import *

class Parser:
    __lex = None
    __token = None

    def __init__(self, filepath):
        self.__lex = Lexer(filepath)
        self.__token = None

        self.__firstPrimaryExpression = set((Tag.ID, Tag.NUMBER, Tag.TRUE, 
                                             Tag.FALSE, ord('(')))

        self.__firstUnaryExpression = self.__firstPrimaryExpression.union( 
            set((ord('-'), ord('!'))) )
        
        self.__firstExtendedMultiplicativeExpression = set((ord('*'), 
                                                            ord('/'), Tag.MOD))

        self.__firstMultiplicativeExpression = self.__firstUnaryExpression.union(self.__firstExtendedMultiplicativeExpression)
    
        self.__firstExtendedAdditiveExpression = set((ord('+'), ord('-')))
        self.__firstAdditiveExpression = self.__firstMultiplicativeExpression.union(self.__firstExtendedAdditiveExpression)
        
        self.__firstExtendedRelationalExpression = set((ord('<'), ord('>'), Tag.LEQ, Tag.GEQ))
        self.__firstRelationalExpression = self.__firstAdditiveExpression.union(self.__firstExtendedRelationalExpression)
        
        self.__firstExtendedEqualityExpression = set((ord('='), Tag.NEQ))
        self.__firstEqualityExpression = self.__firstRelationalExpression.union(self.__firstExtendedEqualityExpression)
        
        self.__firstExtendedConditionalTerm = {Tag.AND}
        self.__firstConditionalTerm = self.__firstEqualityExpression.union(self.__firstExtendedConditionalTerm)

        self.__firstExtendedConditionalExpression = {Tag.OR}
        self.__firstConditionalExpression = self.__firstConditionalTerm.union(self.__firstExtendedConditionalExpression)

        
        self.__firstExpression = self.__firstConditionalExpression
        self.__firstIfStatement = {Tag.IF}
        self.__firstIfElseStatement = {Tag.IFELSE}
        self.__firstConditionalStatement = self.__firstIfStatement.union(self.__firstIfElseStatement)
        self.__firstRepetitiveStatement = {Tag.WHILE}
        self.__firstStructuredStatement = self.__firstConditionalStatement.union(self.__firstRepetitiveStatement)
        self.__firstElementList = {ord(',')}
        self.__firstElement = self.__firstExpression.union({Tag.STRING})
        self.__firstTextStatement = {Tag.PRINT}
        self.__firstPenwidthStatement = {Tag.PENWIDTH}
        self.__firstColorStatement = {Tag.COLOR}
        self.__firstPendownStatement = {Tag.PENDOWN}
        self.__firstPenupStatement = {Tag.PENUP}
        self.__firstArcStatement = {Tag.ARC}
        self.__firstCircleStatement = {Tag.CIRCLE}
        self.__firstClearStatement =  {Tag.CLEAR}
        self.__firstDrawingStatement = self.__firstClearStatement.union(self.__firstCircleStatement.union(self.__firstArcStatement.union(self.__firstPenupStatement.union(self.__firstPendownStatement.union(self.__firstColorStatement.union(self.__firstPenwidthStatement))))))
        self.__firstForwardStatement =  {Tag.FORWARD}
        self.__firstBackwardStatement =  {Tag.BACKWARD}
        self.__firstRightStatement = {Tag.RIGHT}
        self.__firstLeftStatement = {Tag.LEFT}
        self.__firstSetxStatement = {Tag.SETX}
        self.__firstSetyStatement = {Tag.SETY}
        self.__firstSetxyStatement = {Tag.SETXY}
        self.__firstMovementStatement = self.__firstForwardStatement.union(self.__firstBackwardStatement.union(self.__firstRightStatement.union(self.__firstLeftStatement.union(self.__firstSetxStatement.union(self.__firstSetyStatement.union(self.__firstSetxyStatement.union({Tag.HOME})))))))
        self.__firstAssigmentStatement = {Tag.ID}
        self.__firstSimpleStatement = self.__firstAssigmentStatement.union(self.__firstMovementStatement.union(self.__firstDrawingStatement.union(self.__firstTextStatement)))
        self.__firstStatement = self.__firstSimpleStatement.union(self.__firstStructuredStatement)
        self.__firstStatementSequence = self.__firstStatement
        self.__firstIdentifierList = {ord(',')}
        self.__firstDeclarationStatement = {Tag.VAR}
        self.__firstProgram = self.__firstStatementSequence.union(self.__firstDeclarationStatement)




    def error(self, extra = None):
        text = 'Line ' + str(self.__lex.getLine()) + " - " 
        if extra == None:
            text = text + "."
        else:
            text = text + extra
        raise Exception(text)

    def __check(self, tag):
        if self.__token.getTag() == tag:
            self.__token = self.__lex.scan()
        else:
            text = 'Line ' + str(self.__lex.getLine()) + " - expected "
            if tag != Tag.ID:
                text = text + str(Token(tag)) + " before " + str(self.__token) 
            else:
                text = text + "an identifier before " + str(self.__token) 
            raise Exception(text)
    
    def analize(self):
        self.__token = self.__lex.scan()
        self.__program()
        
            
            

    def __identifierList(self):
        if self.__token.getTag() in self.__firstIdentifierList:
            self.__check(ord(","))
            self.__check(Tag.ID)
            self.__identifierList()
        else:
            pass     


            
    def __statement(self):
        if self.__token.getTag() in self.__firstStatement:
            if self.__token.getTag() in self.__firstSimpleStatement:
                self.__simpleStatement()
            elif self.__token.getTag() in self.__firstStructuredStatement:
                self.__structured_statement()
        else:
            self.error("ERROR")


    def __simpleStatement(self):
        if self.__token.getTag() in self.__firstSimpleStatement:
            if self.__token.getTag() == Tag.ID:
                self.__assigmentStatement()
            elif self.__token.getTag() in self.__firstMovementStatement:
                self.__movementStatement()
            elif self.__token.getTag() in self.__firstDrawingStatement:
                self.__drawingStatement()
            elif self.__token.getTag() in self.__firstTextStatement:
                self.__text_statement()
        else:
            self.error("ERROR")

    def __assigmentStatement(self):
        if self.__token.getTag() in self.__firstAssigmentStatement:
            self.__check(Tag.ID)
            self.__check(Tag.ASSIGN)
            self.__expression()
        else:
            self.error("ERROR")



    def __movementStatement(self):
        if self.__token.getTag() in self.__firstMovementStatement:
            if self.__token.getTag() == Tag.FORWARD:
                self.__forward_statement()
            elif self.__token.getTag() == Tag.BACKWARD:
                self.__backward_statement()
            elif self.__token.getTag() == Tag.RIGHT:
                self.__right_statement()
            elif self.__token.getTag() == Tag.LEFT:
                self.__left_statement()
            elif self.__token.getTag() == Tag.SETX:
                self.__setx_statement()
            elif self.__token.getTag() == Tag.SETY:
                self.__sety_statement()
            elif self.__token.getTag() == Tag.SETXY:
                self.__setxy_statement()
            elif self.__token.getTag() == Tag.HOME:
                self.__check(Tag.HOME)
        else:
            self.error("ERROR")


    def __forward_statement(self):
        if self.__token.getTag() in self.__firstForwardStatement:
            self.__check(Tag.FORWARD)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __backward_statement(self):
        if self.__token.getTag() in self.__firstBackwardStatement:
            self.__check(Tag.BACKWARD)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __right_statement(self):
        if self.__token.getTag() in self.__firstRightStatement:
            self.__check(Tag.RIGHT)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __left_statement(self):
        if self.__token.getTag() in self.__firstLeftStatement:
            self.__check(Tag.LEFT)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __setx_statement(self):
        if self.__token.getTag() in self.__firstSetxStatement:
            self.__check(Tag.SETX)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __sety_statement(self):
        if self.__token.getTag() in self.__firstSetyStatement:
            self.__check(Tag.SETY)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __setxy_statement(self):
        if self.__token.getTag() in self.__firstSetxyStatement:
            self.__check(Tag.SETXY)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(','))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")
            
    def __drawingStatement(self):
        if self.__token.getTag() in self.__firstDrawingStatement:
            if self.__token.getTag() == Tag.CLEAR:
                self.__clear_statement()
            elif self.__token.getTag() == Tag.CIRCLE:
                self.__circle_statement()
            elif self.__token.getTag() == Tag.ARC:
                self.__arc_statement()
            elif self.__token.getTag() == Tag.PENUP:
                self.__penup_statement()
            elif self.__token.getTag() == Tag.PENDOWN:
                self.__pendown_statement()
            elif self.__token.getTag() == Tag.COLOR:
                self.__color_statement()
            elif self.__token.getTag() == Tag.PENWIDTH:
                self.__penwidth_statement()
        else:
            self.error("ERROR")
     
    def __clear_statement(self):
        if self.__token.getTag() in self.__firstClearStatement:
            self.__check(Tag.CLEAR)
            self.__check('(')
            self.__check(')')
        else:
            self.error("ERROR")
            
    def __circle_statement(self):
        if self.__token.getTag() in self.__firstCircleStatement:
            self.__check(Tag.CIRCLE)
            self.__check('(')
            self.__expression()
            self.__check(')')
        else:
            self.error("ERROR")

    def __arc_statement(self):
        if self.__token.getTag() in self.__firstArcStatement:
            self.__check(Tag.ARC)
            self.__check('(')
            self.__expression()
            self.__check(',')
            self.__expression()
            self.__check(')')
        else:
            self.error("ERROR")

    def __penup_statement(self):
        if self.__token.getTag() in self.__firstPenupStatement:
            self.__check(Tag.PENUP)
            self.__check('(')
            self.__check(')')
        else:
            self.error("ERROR")

    def __pendown_statement(self):
        if self.__token.getTag() in self.__firstPendownStatement:
            self.__check(Tag.PENDOWN)
            self.__check('(')
            self.__check(')')
        else:
            self.error("ERROR")

    def __color_statement(self):
        if self.__token.getTag() in self.__firstColorStatement:
            self.__check(Tag.COLOR)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(','))
            self.__expression()
            self.__check(ord(','))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __penwidth_statement(self):
        if self.__token.getTag() in self.__firstPenwidthStatement:
            self.__check(Tag.PENWIDTH)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    def __text_statement(self):
        if self.__token.getTag() in self.__firstTextStatement:
            self.__check(Tag.PRINT)
            self.__check(ord('('))
            self.__element()
            self.__element_list()
            self.__check(ord(')'))
        else:
            self.error("ERROR")

    
    def __element(self):
        if self.__token.getTag() in self.__firstElement:
            if self.__token.getTag() == Tag.STRING:
                self.__check(Tag.STRING)
            elif self.__token.getTag() in self.__firstExpression:
                self.__expression()
        else:
            self.error("ERROR")

    def __element_list(self):
        if self.__token.getTag() in self.__firstElementList:
            self.__check(ord(','))
            self.__element()
            self.__element_list()
        else:
            pass

    def __structured_statement(self):
        if self.__token.getTag() in self.__firstStructuredStatement:
            if self.__token.getTag() == Tag.WHILE:
                self.__repetitive_statement()
            elif self.__token.getTag() in self.__firstConditionalStatement:
                self.__conditional_statement()
        else:
            self.error("ERROR")
    
    
    def __repetitive_statement(self):
        if self.__token.getTag() in self.__firstRepetitiveStatement:
            self.__check(Tag.WHILE)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
            self.__check(ord('['))
            self.__statementSequence()
            self.__check(ord(']'))
        else:
            self.error("ERROR")

    def __conditional_statement(self):
        if self.__token.getTag() in self.__firstConditionalStatement:
            if self.__token.getTag() == Tag.IF:
                self.__if_statement()
            elif self.__token.getTag() == Tag.IFELSE:
                self.__if_else_statement()
        else:
            self.error("ERROR")


    def __if_statement(self):
        if self.__token.getTag() in self.__firstIfStatement:
            self.__check(Tag.IF)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
            self.__check(ord('['))
            self.__statementSequence()
            self.__check(ord(']'))
        else:
            self.error("ERROR")

    def __if_else_statement(self):
         if self.__token.getTag() in self.__firstIfElseStatement:
            self.__check(Tag.IFELSE)
            self.__check(ord('('))
            self.__expression()
            self.__check(ord(')'))
            self.__check(ord('['))
            self.__statementSequence()
            self.__check(ord(']'))
            self.__check(ord('['))
            self.__statementSequence()
            self.__check(ord(']'))
         else:
            self.error("ERROR")
    
    def __expression(self):
        if self.__token.getTag() in self.__firstExpression:
            self.__conditionalExpression()
        else:
            self.error("ERROR")
    
    
    def __conditionalExpression(self):
        if self.__token.getTag() in self.__firstConditionalExpression:
            self.__conditionalTerm()
            self.__extendedConditionalExpression()
        else:
            self.error("ERROR")        
            
    def __extendedConditionalExpression(self):
        if self.__token.getTag() in self.__firstExtendedConditionalExpression:
            self.__check(Tag.OR)
            self.__conditionalTerm()
            self.__extendedConditionalExpression()
        else:
            pass   
            
    def __conditionalTerm(self):
        if self.__token.getTag() in self.__firstConditionalTerm:
            self.__equalityExpression()
            self.__extendedConditionalTerm()
        else:
            self.error("ERROR") 
    
    def __extendedConditionalTerm(self):
        if self.__token.getTag() in self.__firstExtendedConditionalTerm:
            if self.__token.getTag() == Tag.AND:
                self.__check(Tag.AND)
                self.__equalityExpression()
                self.__extendedConditionalTerm()
        else:
            pass
    
            
    def __equalityExpression(self):
        if self.__token.getTag() in self.__firstEqualityExpression:
            self.__relationalExpression()
            self.__extendedEqualityExpression()
        else:
            self.error("ERROR")
 
    def __extendedEqualityExpression(self):
        if self.__token.getTag() in self.__firstExtendedEqualityExpression:
            if self.__token.getTag() == ord('='):
                self.__check(ord('='))
                self.__relationalExpression()
                self.__extendedEqualityExpression()
            elif self.__token.getTag() == Tag.NEQ:
                self.__check(Tag.NEQ)
                self.__relationalExpression()
                self.__extendedEqualityExpression()
        else:
            pass
            
    def __relationalExpression(self):
        if self.__token.getTag() in self.__firstRelationalExpression:
            self.__additiveExpression()
            self.__extendedRelationalExpression()
        else:
            self.error("ERROR")
            
    def __extendedRelationalExpression(self):
        if self.__token.getTag() in self.__firstExtendedRelationalExpression:
            if self.__token.getTag() == ord('<'):
                self.__check(ord('<'))
                self.__additiveExpression()
                self.__extendedRelationalExpression()
            elif self.__token.getTag() == Tag.LEQ:
                self.__check(Tag.LEQ)
                self.__additiveExpression()
                self.__extendedRelationalExpression()
            elif self.__token.getTag() == ord('>'):
                self.__check(ord('>'))
                self.__additiveExpression()
                self.__extendedRelationalExpression()
            elif self.__token.getTag() == Tag.GEQ:
                self.__check(Tag.GEQ)
                self.__additiveExpression()
                self.__extendedRelationalExpression()
        else:
            pass
            
    def __additiveExpression(self):
        if self.__token.getTag() in self.__firstAdditiveExpression:
            self.__multiplicativeExpression()
            self.__extendedAdditiveExpression()
        else:
            self.error("ERROR")

    def __extendedAdditiveExpression(self):
        if self.__token.getTag() in self.__firstExtendedAdditiveExpression:
            if self.__token.getTag() == ord('+'):
                self.__check(ord('+'))
                self.__multiplicativeExpression()
                self.__extendedAdditiveExpression()
            elif self.__token.getTag() == ord('-'):
                self.__check(ord('-'))
                self.__multiplicativeExpression()
                self.__extendedAdditiveExpression()
        else:
            pass

    def __multiplicativeExpression(self):
        if self.__token.getTag() in self.__firstMultiplicativeExpression:
            self.__unaryExpression()
            self.__extendedMultiplicativeExpression()
        else:
            self.error("ERROR") 
            
    def __extendedMultiplicativeExpression(self):
        if self.__token.getTag() in self.__firstExtendedMultiplicativeExpression:
            if self.__token.getTag() == ord('*'):
                self.__check(ord('*'))
                self.__unaryExpression()
                self.__extendedMultiplicativeExpression()
            elif self.__token.getTag() == ord('/'):
                self.__check(ord('/'))
                self.__unaryExpression()
                self.__extendedMultiplicativeExpression()
            elif self.__token.getTag() == Tag.MOD:
                self.__check(Tag.MOD)
                self.__unaryExpression()
                self.__extendedMultiplicativeExpression()
        else:
            pass

    def __unaryExpression(self):
        if self.__token.getTag() in self.__firstUnaryExpression:
            if self.__token.getTag() == ord('-'):
                self.__check(ord('-'))
                self.__unaryExpression()
            elif self.__token.getTag() == ord('!'):
                self.__check(ord('!'))
                self.__unaryExpression()
            else:
                self.__primaryExpression()
        else: 
            self.error('Syntax Error')
        
    def __primaryExpression(self):
        if self.__token.getTag() in self.__firstPrimaryExpression:
            if self.__token.getTag() == Tag.ID:
                self.__check(Tag.ID)
            elif self.__token.getTag() == Tag.NUMBER:
                self.__check(Tag.NUMBER)
            elif self.__token.getTag() == Tag.TRUE:
                self.__check(Tag.TRUE)
            elif self.__token.getTag() == Tag.FALSE:
                self.__check(Tag.FALSE)
            elif self.__token.getTag() == ord('('):
                self.__check(ord('('))
                self.__expression()
                self.__check(ord(')'))
        else:
            self.error('Syntax Error')
            
    def __statementSequence(self):
        if self.__token.getTag() in self.__firstStatementSequence:
            self.__statement()
            self.__statementSequence()
        else:
            pass
        
    def __declarationStatement(self):
        if self.__token.getTag() in self.__firstDeclarationStatement:
            self.__check(Tag.VAR)
            self.__check(Tag.ID)
            self.__identifierList()
        else:
            self.error("ERROR")
            
    def __program(self):
        if self.__token.getTag() in self.__firstProgram:
            self.__declarationStatement()
            self.__statementSequence()
        else:
            self.error("ERROR")