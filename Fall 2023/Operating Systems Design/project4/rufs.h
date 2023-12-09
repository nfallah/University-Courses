/*
 *  Copyright (C) 2023 CS416 Rutgers CS
 *	Tiny File System
 *
 *	File:	rufs.h
 *
 */

#include <linux/limits.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdarg.h> // User defined import

#ifndef _TFS_H
#define _TFS_H

#define MAGIC_NUM 0x5C3A
#define MAX_INUM 1024
#define MAX_DNUM 16384

/*
 * User-defined headers
 */

#define DIRECTORY 0
#define FILE 1

#define FALSE 0
#define TRUE 1

#define ROOT_INO 0

#define DEBUG FALSE

struct superblock {
	uint32_t	magic_num;			/* magic number */
	uint16_t	max_inum;			/* maximum inode number */
	uint16_t	max_dnum;			/* maximum data block number */
	uint32_t	i_bitmap_blk;		/* start block of inode bitmap */
	uint32_t	d_bitmap_blk;		/* start block of data block bitmap */
	uint32_t	i_start_blk;		/* start block of inode region */
	uint32_t	d_start_blk;		/* start block of data block region */
};

struct inode {
	uint16_t	ino;				/* inode number */
	uint16_t	valid;				/* validity of the inode */
	uint32_t	size;				/* size of the file */
	uint32_t	type;				/* type of the file */
	uint32_t	link;				/* link count */
	int			direct_ptr[16];		/* direct pointer to data block */
	int			indirect_ptr[8];	/* indirect pointer to data block */
	struct stat	vstat;				/* inode stat */
};

struct dirent {
	uint16_t ino;					/* inode number of the directory entry */
	uint16_t valid;					/* validity of the directory entry */
	char name[208];					/* name of the directory entry */
	uint16_t len;					/* length of name */
};

/*
 * bitmap operations
 */

typedef unsigned char* bitmap_t;

void set_bitmap(bitmap_t b, int i) {
    b[i / 8] |= 1 << (i & 7);
}

void unset_bitmap(bitmap_t b, int i) {
    b[i / 8] &= ~(1 << (i & 7));
}

uint8_t get_bitmap(bitmap_t b, int i) {
    return b[i / 8] & (1 << (i & 7)) ? 1 : 0;
}

/*
 * helper functions (user-defined)
 */

typedef unsigned char boolean;

// Returns an instantiation of the superblock written from the disk.
// Status: COMPLETE
struct superblock *get_superblock() {
	size_t superblock_block_size = (sizeof(struct superblock) + BLOCK_SIZE - 1) / BLOCK_SIZE;
	struct superblock *superblock = malloc(superblock_block_size * BLOCK_SIZE);
	if (!superblock) return NULL;
	struct superblock *superblock_real = malloc(sizeof(struct superblock));
	if (!superblock_real) {
		free(superblock);
		return NULL;
	}
	if (bio_read_multi(0, superblock_block_size, (void *)superblock) != EXIT_SUCCESS) {
		free(superblock);
		free(superblock_real);
		return NULL;
	}
	memcpy(superblock_real, superblock, sizeof(struct superblock));
	free(superblock);
	return superblock_real; // Must be manually freed by the user.
}

// Returns an instantiation of the inode bitmap written from the disk.
// Status: COMPLETE
bitmap_t get_inode_bitmap(struct superblock *superblock) {
	if (!superblock) return NULL;
	size_t inode_bitmap_byte_size = (superblock->max_inum + 7) / 8,
		inode_bitmap_block_size = (inode_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t inode_bitmap = malloc(inode_bitmap_block_size * BLOCK_SIZE);
	if (!inode_bitmap) return NULL;
	bitmap_t inode_bitmap_real = malloc(inode_bitmap_byte_size);
	if (!inode_bitmap_real) {
		free(inode_bitmap);
		return NULL;
	}
	if (bio_read_multi(superblock->i_bitmap_blk, inode_bitmap_block_size, inode_bitmap) != EXIT_SUCCESS) {
		free(inode_bitmap);
		free(inode_bitmap_real);
		return NULL;
	}
	memcpy(inode_bitmap_real, inode_bitmap, inode_bitmap_byte_size);
	free(inode_bitmap);
	return inode_bitmap_real;
}

// Writes the parametrized inode bitmap to the disk, which can also be optionally freed.
// Status: COMPLETE
int update_inode_bitmap(bitmap_t inode_bitmap, boolean free_bitmap, struct superblock *superblock) {
	// Note that inode_bitmap has a precise size (i.e., not rounded up to the nearest block).
	if (!superblock) return -1;
	size_t inode_bitmap_byte_size = (superblock->max_inum + 7) / 8,
		inode_bitmap_block_size = (inode_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t inode_bitmap_real = malloc(inode_bitmap_block_size * BLOCK_SIZE);
	if (!inode_bitmap_real) return -1;
	memset(inode_bitmap_real, 0, inode_bitmap_block_size * BLOCK_SIZE);
	memcpy(inode_bitmap_real, inode_bitmap, inode_bitmap_byte_size);
	if (bio_write_multi(superblock->i_bitmap_blk, inode_bitmap_block_size, inode_bitmap_real) != EXIT_SUCCESS) {
		free(inode_bitmap_real);
		return -1;
	}
	if (free_bitmap == TRUE) free(inode_bitmap);
	free(inode_bitmap_real);
	return EXIT_SUCCESS;
}

// Additional implementation of get_avail_blkno() that does not write to the disk.
// Status: COMPLETE
int get_avail_ino_no_wr(bitmap_t inode_bitmap, struct superblock *superblock) {
	// Note that inode_bitmap must be externally freed.
    if (!inode_bitmap) return -1;
    size_t inode_bitmap_byte_size = (superblock->max_inum + 7) / 8;
    for (unsigned int i = 0; i < inode_bitmap_byte_size; i++) {
		if (inode_bitmap[i] == 255) continue;
		for (int j = 0; j < 8; j++) {
			if (get_bitmap(inode_bitmap, i * 8 + j) == FALSE) {
				set_bitmap(inode_bitmap, i * 8 + j);
				return i * 8 + j;
			}
		}
    }
    return -1;
}

// Returns an instantiation of the data bitmap written from the disk.
// Status: COMPLETE
bitmap_t get_data_bitmap(struct superblock *superblock) {
	if (!superblock) return NULL;
	size_t data_bitmap_byte_size = (superblock->max_dnum + 7) / 8,
		data_bitmap_block_size = (data_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t data_bitmap = malloc(data_bitmap_block_size * BLOCK_SIZE);
	if (!data_bitmap) return NULL;
	bitmap_t data_bitmap_real = malloc(data_bitmap_byte_size);
	if (!data_bitmap_real) {
		free(data_bitmap);
		return NULL;
	}
	if (bio_read_multi(superblock->d_bitmap_blk, data_bitmap_block_size, data_bitmap) != EXIT_SUCCESS) {
		free(data_bitmap);
		free(data_bitmap_real);
		return NULL;
	}
	memcpy(data_bitmap_real, data_bitmap, data_bitmap_byte_size);
	free(data_bitmap);
	return data_bitmap_real;
}

// Writes the parametrized data bitmap to the disk, which can also be optionally freed.
// Status: COMPLETE
int update_data_bitmap(bitmap_t data_bitmap, boolean free_bitmap, struct superblock *superblock) {
	// Note that data_bitmap has a precise size (i.e., not rounded up to the nearest block).
	if (!superblock) return -1;
	size_t data_bitmap_byte_size = (superblock->max_dnum + 7) / 8,
		data_bitmap_block_size = (data_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t data_bitmap_real = malloc(data_bitmap_block_size * BLOCK_SIZE);
	if (!data_bitmap_real) return -1;
	memset(data_bitmap_real, 0, data_bitmap_block_size * BLOCK_SIZE);
	memcpy(data_bitmap_real, data_bitmap, data_bitmap_byte_size);
	if (bio_write_multi(superblock->d_bitmap_blk, data_bitmap_block_size, data_bitmap_real) != EXIT_SUCCESS) {
		free(data_bitmap_real);
		return -1;
	}
	if (free_bitmap == TRUE) free(data_bitmap);
	free(data_bitmap_real);
	return EXIT_SUCCESS;
}

// Additional implementation of get_avail_blkno() that does not write to the disk.
// Status: COMPLETE
int get_avail_blkno_no_wr(bitmap_t data_bitmap, struct superblock *superblock) {
	// Note that data_bitmap must be externally freed.
    if (!data_bitmap) return -1;
    size_t data_bitmap_byte_size = (superblock->max_dnum + 7) / 8;
    for (unsigned int i = 0; i < data_bitmap_byte_size; i++) {
		if (data_bitmap[i] == 255) continue;
		for (int j = 0; j < 8; j++) {
			if (get_bitmap(data_bitmap, i * 8 + j) == FALSE) {
				set_bitmap(data_bitmap, i * 8 + j);
				return i * 8 + j;
			}
		}
    }
    return -1;
}

// Returns the next entry from the path
// Status: COMPLETE
int split_string(int start_ind, const char *path) {
	if (!path || start_ind > strlen(path) + 1 || path[start_ind] == '\0') return -1;
	int curr_ind = start_ind;
	while (path[curr_ind] != '/' && path[curr_ind] != '\0') curr_ind++;
	return curr_ind;
}

// Basic min implementation.
// Status: COMPLETE
int min(int a, int b) { return a < b ? a : b; }

// Basic max implementation.
// Status: COMPLETE
int max(int a, int b) { return a > b ? a : b; }

// Simple print wrapper that only executes if the debug flag is set.
// Status: COMPLETE
void debug(const char *format, ...) {
	if (DEBUG == FALSE) return;
	va_list args;
	va_start(args, format);
	vprintf(format, args);
	va_end(args);
}

#endif