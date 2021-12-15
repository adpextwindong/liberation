#include <stdio.h>

enum ExprTag {
    LitE,
    AddE,
    SubE,
    MulE
};

typedef struct Expr {
    enum ExprTag Tag;
    union {
        int value;
        struct binExpr {
            struct Expr * left;
            struct Expr * right;
        } b;
    } u;
} Expr;

int main(void){

    Expr lit5 = {
        LitE , { 5 }
    };
    Expr lit6 = {
        LitE , { 6 }
    };

    Expr val = {
        AddE, .u.b = { &lit5 , &lit6  }
    };

    printf("Eval val = %d\n", eval(&val));

    return 0;
}

int eval(Expr * e){
    switch(e->Tag){
        case LitE:
            return e->u.value;
        case AddE:
            return (eval (e->u.b.left)) + (eval (e->u.b.right));
        case SubE:
            return (eval (e->u.b.left)) - (eval (e->u.b.right));
        case MulE:
            return (eval (e->u.b.left)) * (eval (e->u.b.right));
    }
}
