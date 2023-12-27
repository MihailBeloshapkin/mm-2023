#include <stdlib.h>
#include <assert.h>

#include "image.h"
#include "custom_assert.h"
#include "utils.h"

image_p create_image(pixel_coord width, pixel_coord height)
{
  image_t* img = malloc(sizeof(image_t));

  img->width = width;
  img->height = height;
  img->data = malloc(sizeof(pixel_data) * width * height);

  return img;
}

void free_image(image_p picture)
{
  free(picture->data);
  free(picture);
}

void save_pgm(image_p picture, const char* filename)
{
  FILE* to = fopen(filename, "w");
  pixel_data* p = picture->data;

  assert_nequal(to, NULL);

  fprintf(to, "P2\n%u %u\n255\n", picture->width, picture->height);

  for (pixel_coord y = 0; y < picture->height; ++y) {
    for (pixel_coord x = 0; x < picture->width; ++x) {
      fprintf(to, "%u%c", *(p++), x == picture->width - 1 ? '\n' : ' ');
    }
  }

  fclose(to);
}

void assert_dimensions(image_p picture, pixel_coord x, pixel_coord y)
{
  NDUNUSED(picture);
  NDUNUSED(x);
  NDUNUSED(y);

  /*
   * No need to check whether x and y are >= 0 as they are represented by an
   * unsigned integer type
   */
  assert_lt(x, picture->width);
  assert_lt(y, picture->height);
}

void set_pixel(image_p picture, pixel_coord x, pixel_coord y, pixel_data color)
{
  assert_dimensions(picture, x, y);

  picture->data[picture->width * y + x] = color;
}

void fill_random(image_p picture)
{
  for (pixel_coord y = 0; y < picture->height; ++y) {
    for (pixel_coord x = 0; x < picture->width; ++x) {
      set_pixel(picture, x, y, (pixel_data)rand());
    }
  }
}

void fill_white(image_p picture)
{
  for (pixel_coord y = 0; y < picture->height; ++y) {
    for (pixel_coord x = 0; x < picture->width; ++x) {
      set_pixel(picture, x, y, (pixel_data)0xff);
    }
  }
}
