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

int main(void){
    
    //A BTree declaration equivlant to tx in the Haskell example
    struct BTree tx = ((struct BTree) {
        .tag = NODE, .node = ((struct Node){
            &(struct BTree){ .tag = LEAF, .value = 5},
            &(struct BTree){ .tag = LEAF, .value = 6}
        })
    });

    return 0;
}
