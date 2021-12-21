%code top
{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    #include <cstring>
    #include <stack>
    #include <iostream>
    extern Ast ast;

    int yylex();
    int yyerror(char const*);
    ArrayType* arrayType;
    int idx;
    int* arrayValue;
    std::stack<ArrayInit*> array_stk;
    std::stack<StmtNode*> whileStk;
    ArrayInit* top;
    int leftCnt = 0;
    int whileCnt = 0;
    int paramNo = 0;
}

%code requires 
{
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"
}

%union 
{
    int itype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    Type* type;
    SymbolEntry* se;
}

%start Program
%token <strtype> ID STRING
%token <itype> INTEGER
%token IF ELSE WHILE
%token INT VOID
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON LBRACKET RBRACKET COMMA  
%token ADD SUB MUL DIV MOD OR AND LESS LESSEQUAL GREATER GREATEREQUAL ASSIGN EQU NEQ NOT
%token CONST
%token RETURN CONTINUE BREAK

%type<stmttype> Stmts Stmt AssignStmt ExprStmt BlockStmt IfStmt WhileStmt BreakStmt ContinueStmt ReturnStmt DeclStmt FuncDef ConstDeclStmt VarDeclStmt ConstDefList VarDef ConstDef VarDefList FuncFParam FuncFParams MaybeFuncFParams BlankStmt
%type<exprtype> Exp AddExp Cond LOrExp PrimaryExp LVal RelExp LAndExp MulExp ConstExp EqExp UnaryExp InitVal ConstInitVal InitValList ConstInitValList FuncArrayIndices FuncRParams ArrayIndices
%type<type> Type

%precedence THEN
%precedence ELSE
%%

Program: Stmts { ast.setRoot($1); }
    ;

Stmts: Stmt { $$ = $1; }
    | Stmts Stmt{ $$ = new StmtsNode($1, $2); }
    ;

Stmt: AssignStmt { $$ = $1; }
    | ExprStmt   { $$ = $1; }
    | BlockStmt  { $$ = $1; }
    | BlankStmt  { $$ = $1; }
    | IfStmt     { $$ = $1; }
    | WhileStmt  { $$ = $1; }
    | BreakStmt 
    {
        if(!whileCnt)
            fprintf(stderr, "break not match while\n");
        $$ = $1;
    }
    | ContinueStmt 
    {
        if(!whileCnt)
            fprintf(stderr, "continue not match while\n");
        $$ = $1;
    }
    | ReturnStmt { $$ = $1; }
    | DeclStmt   { $$ = $1; }
    | FuncDef    { $$ = $1; }
    ;

LVal: ID 
    {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        if(se == nullptr)
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);
        $$ = new Id(se);
        delete []$1;
    }
    | ID ArrayIndices
    {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        if(se == nullptr)
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);
        $$ = new Id(se, $2);
        delete []$1;
    }
    ; 
AssignStmt
    : LVal ASSIGN Exp SEMICOLON
    {
        $$ = new AssignStmt($1, $3);
    }
    ;
ExprStmt
    : Exp SEMICOLON
    {
        $$ = new ExprStmt($1);
    }
    ;
BlankStmt
    : SEMICOLON
    {
        $$ = new BlankStmt();
    }
    ;
BlockStmt
    : LBRACE
    {
        identifiers = new SymbolTable(identifiers);
    } 
      Stmts RBRACE
      {
        $$ = new CompoundStmt($3);

        SymbolTable* top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
      }
    | LBRACE RBRACE
    {
        $$ = new CompoundStmt();
    }
    ;
IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN
    {
        $$ = new IfStmt($3, $5);
    }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt
    {
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;
WhileStmt
    : WHILE LPAREN Cond RPAREN
    {
        whileCnt++;
        WhileStmt *whileNode = new WhileStmt($3);
        $<stmttype>$ = whileNode;
        whileStk.push(whileNode);
    }
    Stmt
    {
        StmtNode *whileNode = $<stmttype>5; 
        ((WhileStmt*)whileNode)->setStmt($6);
        $$=whileNode;
        whileStk.pop();
        whileCnt--;
    }
    ;
BreakStmt
    : BREAK SEMICOLON
    {
        $$ = new BreakStmt(whileStk.top());
    }
    ;
ContinueStmt
    : CONTINUE SEMICOLON
    {
        $$ = new ContinueStmt(whileStk.top());
    }
    ;
ReturnStmt
    : RETURN SEMICOLON
    {
        $$ = new ReturnStmt();
    }
    | RETURN Exp SEMICOLON
    {
        $$ = new ReturnStmt($2);
    }
    ;
Exp
    :
    AddExp {$$ = $1;}
    ;
Cond
    :
    LOrExp {$$ = $1;}
    ;
PrimaryExp
    : LPAREN Exp RPAREN
    {
        $$ = $2;
    }
    | LVal
    {
        $$ = $1;
    }
    | STRING
    {
        SymbolEntry* se;
        se = globals->lookup(std::string($1));
        if(se == nullptr)
        {
            Type* type = new StringType(strlen($1));
            se = new ConstantSymbolEntry(type, std::string($1));
            globals->install(std::string($1), se);
        }
        ExprNode* expr = new ExprNode(se);

        $$ = expr;
    }
    | INTEGER
    {
        SymbolEntry* se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);
    }
    ;
UnaryExp 
    : PrimaryExp {$$ = $1;}
    | ID LPAREN FuncRParams RPAREN
    {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        if(se == nullptr)
            fprintf(stderr, "function \"%s\" is undefined\n", (char*)$1);
        $$ = new FuncCallExpr(se, $3);
    }
    | ID LPAREN RPAREN
    {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        if(se == nullptr)
            fprintf(stderr, "function \"%s\" is undefined\n", (char*)$1);
        $$ = new FuncCallExpr(se);
    }
    | ADD UnaryExp {$$ = $2;}
    | SUB UnaryExp
      {
          SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
          $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
      }
    | NOT UnaryExp
      {
          SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
          $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
      }
    ;
MulExp
    : UnaryExp {$$ = $1;}
    | MulExp MUL UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
    }
    | MulExp DIV UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    | MulExp MOD UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;
AddExp
    : MulExp {$$ = $1;}
    | AddExp ADD MulExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    | AddExp SUB MulExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;
RelExp
    : AddExp {
        $$ = $1;
    }
    | RelExp LESS AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    | RelExp LESSEQUAL AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESSEQUAL, $1, $3);
    }
    | RelExp GREATER AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
    }
    | RelExp GREATEREQUAL AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, $1, $3);
    }
    ;
EqExp
    : RelExp {$$ = $1;}
    | EqExp EQU RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQU, $1, $3);
    }
    | EqExp NEQ RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NEQ, $1, $3);
    }
    ;
LAndExp
    : EqExp {$$ = $1;}
    | LAndExp AND EqExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;
LOrExp
    : LAndExp {$$ = $1;}
    | LOrExp OR LAndExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;
ConstExp
    : AddExp {$$ = $1;}
    ;
FuncRParams 
    : Exp {$$ = $1;}
    | FuncRParams COMMA Exp {
        $$ = $1;
        $$->setNext($3);
    }
Type
    : INT {
        $$ = TypeSystem::intType;
    }
    | VOID {
        $$ = TypeSystem::voidType;
    }
    ;
DeclStmt
    : VarDeclStmt {$$ = $1;}
    | ConstDeclStmt {$$ = $1;}
    ;
VarDeclStmt
    : Type VarDefList SEMICOLON {$$ = $2;}
    ;
ConstDeclStmt
    : CONST Type ConstDefList SEMICOLON {
        $$ = $3;
    }
    ;
VarDefList
    : VarDefList COMMA VarDef {
        $$ = $1;
        $1->setNext($3);
    } 
    | VarDef {$$ = $1;}
    ;
ConstDefList
    : ConstDefList COMMA ConstDef {
        $$ = $1;
        $1->setNext($3);
    }
    | ConstDef {$$ = $1;}
    ;
VarDef
    : ID {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
        if(!identifiers->install($1, se))
            fprintf(stderr, "identifier \"%s\" is already defined\n", (char*)$1);
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    | ID ArrayIndices {
        SymbolEntry* se;
        std::vector<int> dims;
        ExprNode* temp = $2;
        while(temp)
        {
            dims.push_back(temp->evaluate());
            temp = (ExprNode*)(temp->getNext());
        }
        Type *type = TypeSystem::intType;
        Type* temp1;
        while(!dims.empty())
        {
            temp1 = new ArrayType(type, dims.back());
            if(type->isArray())
                ((ArrayType*)type)->setArrayType(temp1);
            type = temp1;
            dims.pop_back();
        }
        arrayType = (ArrayType*)type;
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        ((IdentifierSymbolEntry*)se)->setAllZero();
        int *p = new int[type->getSize()/32];
        //int *p = new int[arrayType->getLength()];
        //fprintf(stdout,"%d\n",sizeof(*p)/sizeof(int));
        ((IdentifierSymbolEntry*)se)->setArrayValue(p);
        if(!identifiers->install($1, se))
            fprintf(stderr, "<Identifier \"%s\" redefined>\n", (char*)$1);//12.18
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    | ID ASSIGN InitVal {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
        if(!identifiers->install($1, se))
            fprintf(stderr, "<Identifier \"%s\" redefined>\n", (char*)$1);//12.18
        ((IdentifierSymbolEntry*)se)->setValue($3->evaluate());
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    | ID ArrayIndices ASSIGN {
        SymbolEntry* se;
        std::vector<int> dims;
        ExprNode* temp = $2;
        while(temp)
        {
            dims.push_back(temp->evaluate());
            temp = (ExprNode*)(temp->getNext());
        }
        //fprintf(stdout,"####################\n");
        Type* type = TypeSystem::intType;
        Type* temp1;
        while(!dims.empty())
        {
            temp1 = new ArrayType(type, dims.back());
            if(type->isArray())
                ((ArrayType*)type)->setArrayType(temp1);
            type = temp1;
            dims.pop_back();
        }
        arrayType = (ArrayType*)type;
        idx = 0;
        std::stack<ArrayInit*>().swap(array_stk);//将栈清空。
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        $<se>$ = se;
        arrayValue = new int[arrayType->getSize()/32];
        //fprintf(stdout,"Length:%d",arrayType->getLength());
        //arrayValue = new int[arrayType->getLength()];
        //fprintf(stdout,"Length:%d",arrayType->getLength());
        //fprintf(stdout,"####################\n");
    }
      InitVal
    {
        ((IdentifierSymbolEntry*)$<se>4)->setArrayValue(arrayValue);
        if(((ArrayInit*)$5)->isEmpty())//={}
            ((IdentifierSymbolEntry*)$<se>4)->setAllZero();
        if(!identifiers->install($1, $<se>4))
            fprintf(stderr, "<Identifier \"%s\" redefined>\n", (char*)$1);//12.18
        $$ = new DeclStmt(new Id($<se>4), $5);
        delete []$1;
    }
    ;
ConstDef
    : ID ASSIGN ConstInitVal {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry(TypeSystem::constIntType, $1, identifiers->getLevel());
        ((IdentifierSymbolEntry*)se)->setConst();
        if(!identifiers->install($1, se))
            fprintf(stderr, "identifier \"%s\" is already defined\n", (char*)$1);
        identifiers->install($1, se);
        ((IdentifierSymbolEntry*)se)->setValue($3->evaluate());
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    | ID ArrayIndices ASSIGN  {
        SymbolEntry* se;
        std::vector<int> dims;
        ExprNode* temp = $2;
        while(temp){
            dims.push_back(temp->evaluate());
            temp = (ExprNode*)(temp->getNext());
        }
        Type* type = TypeSystem::constIntType;
        Type* temp1;
        for(auto it = dims.rbegin(); it != dims.rend(); it++) {
            temp1 = new ArrayType(type, *it, true);
            if(type->isArray())
                ((ArrayType*)type)->setArrayType(temp1);
            type = temp1;
        }
        arrayType = (ArrayType*)type;
        idx = 0;
        std::stack<ArrayInit*>().swap(array_stk);
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        ((IdentifierSymbolEntry*)se)->setConst();
        $<se>$ = se;
        arrayValue = new int[arrayType->getSize()];
    }
      ConstInitVal {
        ((IdentifierSymbolEntry*)$<se>4)->setArrayValue(arrayValue);
        if(!identifiers->install($1, $<se>4))
            fprintf(stderr, "identifier \"%s\" is already defined\n", (char*)$1);
        identifiers->install($1, $<se>4);
        $$ = new DeclStmt(new Id($<se>4), $5);
        delete []$1;
    } 
    ;
ArrayIndices
    : LBRACKET ConstExp RBRACKET {
        $$ = $2;
    }
    | ArrayIndices LBRACKET ConstExp RBRACKET {
        $$ = $1;
        $1->setNext($3);
    }
    ;
InitVal 
    : Exp {
        if(!$1->getType()->isInt())
            fprintf(stderr, "cannot initialize a variable of type \'int\' with an rvalue of type \'%s\'\n", $1->getType()->toStr().c_str());
        $$ = $1;
        //fprintf(stdout,"#############\n");
        if(!array_stk.empty())
        {
            arrayValue[idx++] = $1->evaluate();
            Type* arrTy = array_stk.top()->getSymbolEntry()->getType();
            if(arrTy == TypeSystem::intType)
                array_stk.top()->addExpr($1);
            else
                while(arrTy)//高维度
                {
                    //fprintf(stdout,"#################\n");
                    if(((ArrayType*)arrTy)->getElementType() != TypeSystem::intType)//不是int，一直往低维度找
                    {
                        //fprintf(stdout,"@@@@@@@@@@@@@@@@\n");
                        arrTy = ((ArrayType*)arrTy)->getElementType();
                        SymbolEntry* se = new ConstantSymbolEntry(arrTy);
                        ArrayInit* list = new ArrayInit(se);
                        array_stk.top()->addExpr(list);
                        array_stk.push(list);
                        //fprintf(stdout,"push\n");//~~
                    }
                    else
                    {
                        //fprintf(stdout,"###############\n");
                        array_stk.top()->addExpr($1);
                        /*bool full=false;
                        if(array_stk.top()->isFull())
                        {
                            full=true;
                            fprintf(stdout,"@@@@@@@@@@@@@@@@\n");
                        }*/
                        while(array_stk.top()->isFull() && array_stk.size() != (long unsigned int)leftCnt)
                        {
                            /*if(full&&array_stk.size() != (long unsigned int)leftCnt)
                                fprintf(stdout,"#######################\n");*/
                            arrTy = ((ArrayType*)arrTy)->getArrayType();
                            array_stk.pop();
                            //fprintf(stdout,"pop\n");//pipapo
                        }
                        break;
                    }
                }
        }         
    }
    | LBRACE RBRACE {
        SymbolEntry* se;
        ExprNode* list;
        if(array_stk.empty())//={}
        {
            //fprintf(stdout,"##############\n");
            memset(arrayValue, 0, arrayType->getSize()/32);
            //memset(arrayValue, 0, arrayType->getLength());
            idx += arrayType->getSize() / TypeSystem::intType->getSize();
            se = new ConstantSymbolEntry(arrayType);
            list = new ArrayInit(se);
        }
        else//={...,{},...}
        {
            //fprintf(stdout,"@@@@@@@@@@@@@@@@@\n");
            Type* type = ((ArrayType*)(array_stk.top()->getSymbolEntry()->getType()))->getElementType();// {...,{},...}里的{}的维度
            //ArrayType *arr=(ArrayType*)(type);
            int len = type->getSize() / TypeSystem::intType->getSize();
            memset(arrayValue + idx, 0, type->getSize()/32);
            //memset(arrayValue + idx, 0, arr->getLength());
            idx += len;
            se = new ConstantSymbolEntry(type);
            list = new ArrayInit(se);
            array_stk.top()->addExpr(list);
            while(array_stk.top()->isFull() && array_stk.size() != (long unsigned int)leftCnt)
                array_stk.pop();
        }
        $$ = list;
    }
    | LBRACE {
        //fprintf(stdout,"1\n");
        SymbolEntry* se;
        if(!array_stk.empty())
            arrayType = (ArrayType*)(((ArrayType*)(array_stk.top()->getSymbolEntry()->getType()))->getElementType());
        se = new ConstantSymbolEntry(arrayType);
        if(arrayType->getElementType() != TypeSystem::intType)
            arrayType = (ArrayType*)(arrayType->getElementType());
        ArrayInit* expr = new ArrayInit(se);
        if(!array_stk.empty())
            array_stk.top()->addExpr(expr);
        array_stk.push(expr);
        //fprintf(stdout,"push\n");//~~
        $<exprtype>$ = expr;
        leftCnt++;
        //fprintf(stdout,"1\n");
    } 
      InitValList RBRACE {
        leftCnt--;
        while(array_stk.top() != $<exprtype>2 && array_stk.size() > (long unsigned int)(leftCnt + 1))//否则,出现 {...{{}}} 的例子时会段错误。
        {
            //fprintf(stdout,"%d\n",array_stk.top()->getExpr()->getType()->getSize());
            array_stk.pop();
            //fprintf(stdout,"##################\n");
            //fprintf(stdout,"%d\n",array_stk.top()->getExpr()->getType()->getSize());
        }
        if(array_stk.top() == $<exprtype>2)//已经回到上一维度，说明已经处理完，pop掉当前维度
        {
            array_stk.pop();
            //fprintf(stdout,"##################\n");
        }
        $$ = $<exprtype>2;
        if(!array_stk.empty())
        {
            //fprintf(stdout,"##################\n");
            while(array_stk.top()->isFull() && array_stk.size() != (long unsigned int)leftCnt)
            {
                //fprintf(stdout,"##################\n");
                array_stk.pop();//说明已经处理完，pop掉当前维度
            }
        }
        int size = ((ArrayType*)($$->getSymbolEntry()->getType()))->getSize()/ TypeSystem::intType->getSize();//当前维度大小
        //fprintf(stdout,"1\n");
        while(idx % size != 0)//当前维度剩下的部分填0
            arrayValue[idx++] = 0;
        //fprintf(stdout,"1\n");
        if(!array_stk.empty())
            arrayType = (ArrayType*)(((ArrayType*)(array_stk.top()->getSymbolEntry()->getType()))->getElementType());
    }
    ;

ConstInitVal
    : ConstExp {
        $$ = $1;
        if(!array_stk.empty())
        {
            arrayValue[idx++] = $1->evaluate();
            Type* arrTy = array_stk.top()->getSymbolEntry()->getType();
            if(arrTy == TypeSystem::constIntType)
                array_stk.top()->addExpr($1);
            else
                while(arrTy)
                {
                    if(((ArrayType*)arrTy)->getElementType() != TypeSystem::constIntType){
                        arrTy = ((ArrayType*)arrTy)->getElementType();
                        SymbolEntry* se = new ConstantSymbolEntry(arrTy);
                        ArrayInit* list = new ArrayInit(se);
                        array_stk.top()->addExpr(list);
                        array_stk.push(list);
                    }
                    else
                    {
                        array_stk.top()->addExpr($1);
                        while(array_stk.top()->isFull() && array_stk.size() != (long unsigned int)leftCnt)
                        {
                            arrTy = ((ArrayType*)arrTy)->getArrayType();
                            array_stk.pop();
                        }
                        break;
                    }
                }
        }
    }
    | LBRACE RBRACE {
        SymbolEntry* se;
        ExprNode* list;
        if(array_stk.empty())
        {
            // 只用一个{}初始化数组。
            // 不入栈。
            memset(arrayValue, 0, arrayType->getSize()/32);
            //memset(arrayValue, 0, arrayType->getLength());
            idx += arrayType->getSize() / TypeSystem::constIntType->getSize();
            se = new ConstantSymbolEntry(arrayType);
            list = new ArrayInit(se);
        }
        else
        {
            // 栈不空，不是只有{}。
            Type* type = ((ArrayType*)(array_stk.top()->getSymbolEntry()->getType()))->getElementType();
            //ArrayType *arr=(ArrayType*)(type);
            int len = type->getSize() / TypeSystem::constIntType->getSize();
            memset(arrayValue + idx, 0, type->getSize()/32);
            //memset(arrayValue + idx, 0, arr->getLength());
            idx += len;
            se = new ConstantSymbolEntry(type);
            list = new ArrayInit(se);
            array_stk.top()->addExpr(list);
            while(array_stk.top()->isFull() && array_stk.size() != (long unsigned int)leftCnt){
                array_stk.pop();
            }
        }
        $$ = list;
    }
    | LBRACE {
        SymbolEntry* se;
        if(!array_stk.empty())
            arrayType = (ArrayType*)(((ArrayType*)(array_stk.top()->getSymbolEntry()->getType()))->getElementType());
        se = new ConstantSymbolEntry(arrayType);
        if(arrayType->getElementType() != TypeSystem::intType){
            arrayType = (ArrayType*)(arrayType->getElementType());
        }
        ArrayInit* expr = new ArrayInit(se);
        if(!array_stk.empty())
            array_stk.top()->addExpr(expr);
        array_stk.push(expr);
        $<exprtype>$ = expr;
        leftCnt++;
    } 
      ConstInitValList RBRACE {
        leftCnt--;
        while(array_stk.top() != $<exprtype>2 && array_stk.size() > (long unsigned int)(leftCnt + 1))
            array_stk.pop();
        if(array_stk.top() == $<exprtype>2)
            array_stk.pop();
        $$ = $<exprtype>2;
        if(!array_stk.empty())
            while(array_stk.top()->isFull() && array_stk.size() != (long unsigned int)leftCnt)
            {
                array_stk.pop();
            }
        while(idx % (((ArrayType*)($$->getSymbolEntry()->getType()))->getSize()/ sizeof(int)) !=0 )
            arrayValue[idx++] = 0;
        if(!array_stk.empty())
            arrayType = (ArrayType*)(((ArrayType*)(array_stk.top()->getSymbolEntry()->getType()))->getElementType());
    }
    ;
InitValList
    : InitVal {
        $$ = $1;
    }
    | InitValList COMMA InitVal {
        $$ = $1;
    }
    ;
ConstInitValList
    : ConstInitVal {
        $$ = $1;
    }
    | ConstInitValList COMMA ConstInitVal {
        $$ = $1;
    }
    ;
FuncDef
    :
    Type ID {
        identifiers = new SymbolTable(identifiers);
        paramNo = 0;
    }
    LPAREN MaybeFuncFParams RPAREN {
        Type* funcType;
        std::vector<Type*> vec;
        std::vector<SymbolEntry*> vec1;
        DeclStmt* temp = (DeclStmt*)$5;
        while(temp){
            vec.push_back(temp->getId()->getSymbolEntry()->getType());
            vec1.push_back(temp->getId()->getSymbolEntry());
            temp = (DeclStmt*)(temp->getNext());
        }
        funcType = new FunctionType($1, vec, vec1);
        SymbolEntry* se = new IdentifierSymbolEntry(funcType, $2, identifiers->getPrev()->getLevel());
        //fprintf(stdout,"pre:%d\n",identifiers->getPrev()->getLevel());
        //fprintf(stdout,"now:%d\n",identifiers->getLevel());
        if(!identifiers->getPrev()->install($2, se)){
            fprintf(stderr, "redefinition of \'%s %s\'\n", $2, se->getType()->toStr().c_str());
        }
        $<se>$ = se; 
    } 
    BlockStmt {
        $$ = new FuncDefStmt($<se>7, (DeclStmt*)$5, $8);
        SymbolTable* top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;
MaybeFuncFParams
    : FuncFParams {$$ = $1;}
    | %empty {$$ = nullptr;}
FuncFParams
    : FuncFParams COMMA FuncFParam {
        $$ = $1;
        $$->setNext($3);
    }
    | FuncFParam {
        $$ = $1;
    }
    ;
FuncFParam
    : Type ID {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel(), paramNo++);
        //fprintf(stdout,"now:%d\n",identifiers->getLevel());
        identifiers->install($2, se);
        ((IdentifierSymbolEntry*)se)->setLabel();
        ((IdentifierSymbolEntry*)se)->setAddr(new Operand(se));
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    | Type ID FuncArrayIndices {
        SymbolEntry* se;
        ExprNode* temp = $3;
        Type* arr = TypeSystem::intType;
        Type* arr1;
        std::stack<ExprNode*> array_stk;
        while(temp)
        {
            array_stk.push(temp);
            //fprintf(stdout,"push\n");//~~
            temp = (ExprNode*)(temp->getNext());
        }
        while(!array_stk.empty()){
            arr1 = new ArrayType(arr, array_stk.top()->evaluate());
            if(arr->isArray())
                ((ArrayType*)arr)->setArrayType(arr1);
            arr = arr1;
            array_stk.pop();
        }
        se = new IdentifierSymbolEntry(arr, $2, identifiers->getLevel(), paramNo++);
        identifiers->install($2, se);
        ((IdentifierSymbolEntry*)se)->setLabel();
        ((IdentifierSymbolEntry*)se)->setAddr(new Operand(se));
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    ;
FuncArrayIndices 
    : LBRACKET RBRACKET {
        $$ = new ExprNode(nullptr);
    }
    | FuncArrayIndices LBRACKET Exp RBRACKET {
        $$ = $1;
        $$->setNext($3);
    }
%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
