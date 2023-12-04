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

#ifndef _TFS_H
#define _TFS_H

#define MAGIC_NUM 0x5C3A
#define MAX_INUM 1024
#define MAX_DNUM 16384

// User-defined; makes inode type differentiation more explicit and convenient
#define DIRECTORY 0
#define FILE 1

// User-defined; makes boolean logic more explicit and convenient
#define FALSE 0
#define TRUE 1

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

struct superblock *get_superblock() {
	size_t superblock_block_size = (sizeof(struct superblock) + BLOCK_SIZE - 1) / BLOCK_SIZE;
	struct superblock *superblock = malloc(sizeof(struct superblock));
	if (!superblock) return NULL;
	if (bio_read_multi(0, superblock_block_size, (void *)superblock) != EXIT_SUCCESS) {
		free(superblock);
		return NULL;
	}
	return superblock;
}

bitmap_t get_inode_bitmap(struct superblock *superblock) {
	if (!superblock) return NULL;
	size_t inode_bitmap_byte_size = (superblock->max_inum + 7) / 8,
		inode_bitmap_block_size = (inode_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t inode_bitmap = malloc(inode_bitmap_byte_size);
	if (!inode_bitmap) {
		return NULL;
	}
	if (bio_read_multi(superblock->i_bitmap_blk, inode_bitmap_block_size, inode_bitmap) != EXIT_SUCCESS) {
		free(inode_bitmap);
		return NULL;
	}
	return inode_bitmap;
}

// Currently unused
int update_inode_bitmap(bitmap_t inode_bitmap, boolean free_bitmap, struct superblock *superblock) {
	if (!superblock) {
		perror("update_inode_bitmap failed");
		return -1;
	}
	size_t inode_bitmap_byte_size = (superblock->max_inum + 7) / 8,
		inode_bitmap_block_size = (inode_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	if (bio_write_multi(superblock->i_bitmap_blk, inode_bitmap_block_size, inode_bitmap) != EXIT_SUCCESS) {
		perror("update_inode_bitmap failed");
		return -1;
	}
	if (free_bitmap) free(inode_bitmap);
	return EXIT_SUCCESS;
}

bitmap_t get_data_bitmap(struct superblock *superblock) {
	if (!superblock) return NULL;
	size_t data_bitmap_byte_size = (superblock->max_dnum + 7) / 8,
		data_bitmap_block_size = (data_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t data_bitmap = malloc(data_bitmap_byte_size);
	if (!data_bitmap) {
		return NULL;
	}
	if (bio_read_multi(superblock->d_bitmap_blk, data_bitmap_block_size, data_bitmap) != EXIT_SUCCESS) {
		free(data_bitmap);
		return NULL;
	}
	return data_bitmap;
}

// Currently unused
int update_data_bitmap(bitmap_t data_bitmap, boolean free_bitmap, struct superblock *superblock) {
	if (!superblock) {
		perror("update_data_bitmap failed");
		return -1;
	}
	size_t data_bitmap_byte_size = (superblock->max_dnum + 7) / 8,
		data_bitmap_block_size = (data_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	if (bio_write_multi(superblock->d_bitmap_blk, data_bitmap_block_size, data_bitmap) != EXIT_SUCCESS) {
		perror("update_data_bitmap failed");
		return -1;
	}
	if (free_bitmap) free(data_bitmap);
	return EXIT_SUCCESS;
}

int min(int a, int b) {
	return a < b ? a : b;
}

int max(int a, int b) {
	return a > b ? a : b;
}

#endif