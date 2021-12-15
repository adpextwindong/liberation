#include <stdbool.h>
enum DelimitMethod {
    None = 0,
    Prepend = 1,
    Separate = 2
};

static bool mode = false;
static enum DelimitMethod method = None;

int main(void){

    //printf("Method: %d\n", method);

    return 0;
}
