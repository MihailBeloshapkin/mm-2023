#ifndef _FRACTAL_H_
#define _FRACTAL_H_

#include "image.h"

/**
 * @brief Enum for fractal types
 */
typedef enum {
  SIERPINSKI,
  MANDELBROT,
} fractal_type;

/**
 * @brief Draws empty fractal, assuming image is clean
 *
 * @param picture   Pointer to the picture where image will be drawn
 * @param type      Type of the fractal to draw
 */
void fractal(image_p picture, fractal_type type);

#endif // _FRACTAL_H_
