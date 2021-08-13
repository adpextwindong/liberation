#include <stdio.h>

//BTree Union Tag
enum BTREE_TAG {
    NODE,
    LEAF
};

//BTree DataType
struct BTree {
    enum BTREE_TAG tag;
    union {
        struct Node {
            struct BTree * left;
            struct BTree * right;
        } node;
        int value;
    };
} BTree;

struct BTree foo(void){
    struct BTree ret = ((struct BTree) {
        .tag = NODE, .node = ((struct Node){
            &(struct BTree){ .tag = LEAF, .value = 5},
            &(struct BTree){ .tag = LEAF, .value = 6}
        })
    });

    return ret;
}
int main(void){
    struct BTree tx = foo();

    printf("%d\n", tx.node.left->value);

    return 0;
}
