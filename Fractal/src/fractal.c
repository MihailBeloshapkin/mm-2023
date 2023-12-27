#include <assert.h>
#include <complex.h>
#include "image.h"
#include "fractal.h"
#include "custom_assert.h"

void draw_sierpinski_carpet(image_p picture, int x, int y, int size)
{
  if (size < 1)
    return;

  int newSize = size / 3;

  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      if (i == 1 && j == 1) {
        for (int k = y + newSize; k < y + 2 * newSize; k++) {
          for (int l = x + newSize; l < x + 2 * newSize; l++) {
            set_pixel(picture, l, k, 0);
          }
        }
      }
      else {
        draw_sierpinski_carpet(picture, x + i * newSize, y + j * newSize,
                               newSize);
      }
    }
  }
}

void draw_mandelbrot_set(image_p picture)
{
  int max_iter = 1000;
  double scale = 4.0 / picture->width;

  for (pixel_coord y = 0; y < picture->height; y++) {
    for (pixel_coord x = 0; x < picture->width; x++) {
      double real = (x - picture->width / 2.0) * scale;
      double imag = (y - picture->height / 2.0) * scale;

      complex double c = real + imag * I;
      complex double z = 0;
      int iter;

      for (iter = 0; iter < max_iter && cabs(z) <= 2.0; iter++) {
        z = z * z + c;
      }

      set_pixel(picture, x, y, iter % 256);
    }
  }
}

void fractal(image_p picture, fractal_type type)
{
  if (type == SIERPINSKI) {
      assert_equal(picture->width, picture->height);

      int size = picture->width;
      draw_sierpinski_carpet(picture, 0, 0, size);
  }
  else if (type == MANDELBROT) {
      draw_mandelbrot_set(picture);
  }
}