#ifndef ADT_H
#define ADT_H
#include <stdio.h>

enum TREE_TAG {
    NODE,
    LEAF
};

struct Tree {
    enum TREE_TAG tag;
    union {
        struct Node {
            struct Tree * left;
            struct Tree * right;
        } node;
        int value;
    };
} Tree;

int main(void){
    struct Tree tx = ((struct Tree) {
        .tag = NODE, .node = ((struct Node){
            &(struct Tree){ .tag = LEAF, .value = 5},
            &(struct Tree){ .tag = LEAF, .value = 6}
        })
    });

    printf("%d\n", tx.node.left->value);
    printf("%d\n", tx.node.right->value);

    return 0;
}

#endif