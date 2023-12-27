#ifndef _IMAGE_H_
#define _IMAGE_H_

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

/**
 * @brief One pixel data in-place
 */
typedef uint8_t pixel_data;

/**
 * @brief Coordinate of a pixel and dimension of an image
 */
typedef unsigned int pixel_coord;

/**
 * @brief Structure to store image data and metadata
 */
typedef struct image {
  pixel_coord width;
  pixel_coord height;
  pixel_data *data;
} image_t;

/**
 * @brief Image pointer
 */
typedef struct image *image_p;

/**
 * @brief Create image of given width and height
 *
 * @param width       Width of the image in pixels
 * @param height      Height of the image in pixels
 *
 * @returns A pointer to the newly created image
 */
image_p create_image(pixel_coord width, pixel_coord height);

/**
 * @brief Fill an image with random pixel data
 *
 * @param picture     Pointer to the image to be filled
 */
void fill_random(image_p picture);

/**
 * @brief Fill an image with white pixels
 *
 * @param picture     Pointer to the image to be filled
 */
void fill_white(image_p picture);

/**
 * @brief Free the memory allocated for an image
 *
 * @param picture     Pointer to the image to be freed
 */
void free_image(image_p picture);

/**
 * @brief Save an image in PGM format
 *
 * @param picture     Pointer to the image to be saved
 * @param filename    Filename to save the image as
 */
void save_pgm(image_p picture, const char *filename);

/**
 * @brief Set the pixel of an image to a given color
 *
 * @param picture     Pointer to the image
 * @param x           X-coordinate of the pixel
 * @param y           Y-coordinate of the pixel
 * @param color       Color value to set the pixel to
 */
void set_pixel(image_p picture, pixel_coord x, pixel_coord y, pixel_data color);

#endif // _IMAGE_H_
