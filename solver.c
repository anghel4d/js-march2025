#include <stdio.h>
#include <stdint.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdbool.h>

#ifdef _MSC_VER
    #define ALWAYS_INLINE __forceinline
#else
    #define ALWAYS_INLINE static inline __attribute__((always_inline))
#endif

#define GRIDSIZE 5

// These are hints along the perimeter of a square grid.
typedef struct {
    uint16_t top[GRIDSIZE];     // read left to right
    uint16_t bottom[GRIDSIZE];  // read left to right
    uint16_t left[GRIDSIZE];    // read top to bottom
    uint16_t right[GRIDSIZE];   // read top to bottom
} Hints;

// This is the mirror grid 
// (as a struct so we can make several and not worry about malloc)
typedef struct {
    // mirror: 0 = none present, 1 = '/', 2 = '\'
    uint8_t mirrors[GRIDSIZE][GRIDSIZE];   // [ROWS][COLS] 
} MirrorGrid;

// These are the directions a beam can travel.
typedef enum {
    UP = 0,
    RIGHT,
    DOWN,
    LEFT
} Direction;

// This is the representation of a laser beam on the grid.
typedef struct {
    Direction dir;
    uint16_t magnitude;
    uint8_t coordinates[2];
} Laser;

ALWAYS_INLINE Direction bounce_helper(Direction dir, uint8_t mirror) {
    
    // lookup tables ↑↓←→
    static const Direction right_bounces[4] = {

        RIGHT,  // [0] coming from up    ╱↑→
        UP,     // [1] coming from right →↑╱
        LEFT,   // [2] coming from down  ←↓╱
        DOWN    // [3] coming from left  ╱↓←
    };
    static const Direction left_bounces[4] = {

        LEFT,   // [0] coming from up    ←↑╲
        UP,     // [1] coming from right →↑╲
        RIGHT,  // [2] coming from down  ╲↓→
        DOWN    // [3] coming from left  ╲↓←
    };

    switch(mirror) {
        case 1: // '/'
            return right_bounces[dir];
        case 2: // '\'
            return left_bounces[dir];
        default:
            return dir; // if no mirror
    };
}

MirrorGrid solver() {
    // 0 = no mirror, // 1 = right-facing, // 2 = right-facing
    MirrorGrid candidate;
    Hints hint = {
        .top    = {0, 0, 9, 0, 0},
        .bottom = {0, 0, 36, 0, 0},
        .left   = {0, 0, 0, 16, 0},
        .right  = {0, 75, 0, 0, 0}
    };

    return candidate;
}

int main() {

    
    printf("Size of mirrorGrid: %lld\n", sizeof(MirrorGrid));
    printf("Size of hints: %lld\n", sizeof(Hints));
    printf("Size of direction: %lld\n", sizeof(Direction));
    return 0;
}

///TODO: runtime type reflection


