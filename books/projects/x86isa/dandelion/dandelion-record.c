#include <assert.h>
#include <memory.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <time.h>
#include <xed/xed-interface.h>

void dump_operand(const xed_operand_t *op, FILE *stream) {
  fprintf(stream, "%s ", xed_operand_enum_t2str(xed_operand_name(op)));
  fprintf(
      stream, "%s ",
      xed_operand_visibility_enum_t2str(xed_operand_operand_visibility(op)));
  fprintf(stream, "%s ", xed_operand_action_enum_t2str(xed_operand_rw(op)));
  fprintf(stream, "%s ", xed_operand_type_enum_t2str(xed_operand_type(op)));
  fprintf(stream, "%s ",
          xed_operand_element_xtype_enum_t2str(xed_operand_xtype(op)));
  if (xed_operand_type(op) == XED_OPERAND_TYPE_NT_LOOKUP_FN)
    fprintf(stream, "%s ",
            xed_nonterminal_enum_t2str(xed_operand_nonterminal_name(op)));
  if (xed_operand_type(op) == XED_OPERAND_TYPE_REG)
    fprintf(stream, "%s ", xed_reg_enum_t2str(xed_operand_reg(op)));
}

void print_hex(uint8_t *buf, size_t n) {
  for (size_t i = 0; i < n; i++)
    printf("%02x ", buf[i]);
}

bool encode_inst(const xed_inst_t *inst, uint8_t buf[15],
                 const xed_reg_enum_t regs[3], xed_state_t *state) {
  unsigned int noperands = xed_inst_noperands(inst);

  // Encode the instruction
  xed_encoder_request_t enc_req;
  xed_encoder_request_zero_set_mode(&enc_req, state);
  xed_encoder_request_set_effective_operand_width(&enc_req, 64);
  xed_encoder_request_set_iclass(&enc_req, xed_inst_iclass(inst));

  // Set operands
  unsigned int n_regs = 0;
  for (unsigned int i = 0; i < noperands; i++) {
    const xed_operand_t *op = xed_inst_operand(inst, i);

    xed_nonterminal_enum_t nonterminal = xed_operand_nonterminal_name(op);
    if (XED_NONTERMINAL_GPR16_B <= nonterminal &&
        nonterminal <= XED_NONTERMINAL_GPRZ_R) {
      assert(n_regs < 3);
      xed_encoder_request_set_reg(&enc_req, xed_operand_name(op),
                                  regs[n_regs++]);
      xed_encoder_request_set_operand_order(&enc_req, i, xed_operand_name(op));
    }

    if (xed_operand_template_is_register(op))
      continue;

    assert(false);
  }

  // Encode
  unsigned int enc_len = 15;  // Initialize to max buffer size
  xed_error_enum_t enc_error = xed_encode(&enc_req, buf, enc_len, &enc_len);

  if (enc_error != XED_ERROR_NONE) {
    fprintf(stderr, "Error %s encoding instruction %s with operands\n",
            xed_error_enum_t2str(enc_error),
            xed_iclass_enum_t2str(xed_inst_iclass(inst)));
    for (unsigned int i = 0; i < noperands; i++) {
      dump_operand(xed_inst_operand(inst, i), stderr);
      fputs("\n", stderr);
    }

    fputs("skipping\n", stderr);

    return true;
  }

  // NOP out the rest of the buffer
  memset(buf + enc_len, 0x90, 15 - enc_len);

  return false;
}

struct inst_context {
  xed_reg_enum_t result_reg;
  uint64_t flags_mask;
  unsigned int n_regs;
};

// It seems silly to encode and instruction and then immediately decode it to
// get information about it. There doesn't seem to be any easy way to get at the
// flags information any other way, so I handle it this way.
void get_inst_context(uint8_t buf[15], struct inst_context *ctx,
                      xed_state_t *state) {
  xed_decoded_inst_t xedd;
  xed_decoded_inst_zero(&xedd);
  xed_decoded_inst_set_mode(&xedd, state->mmode, state->stack_addr_width);

  xed_error_enum_t xed_error = xed_decode(&xedd, buf, 15);
  if (xed_error != XED_ERROR_NONE) {
    return;
  }

  // Count registers and find first written one
  ctx->n_regs = 0;
  ctx->result_reg = XED_REG_INVALID;
  const xed_inst_t *xi = xed_decoded_inst_inst(&xedd);
  unsigned int noperands = xed_inst_noperands(xi);

  for (unsigned int i = 0; i < noperands; i++) {
    const xed_operand_t *op = xed_inst_operand(xi, i);
    xed_reg_enum_t reg = xed_decoded_inst_get_reg(&xedd, xed_operand_name(op));

    if (reg != XED_REG_INVALID && reg != XED_REG_RFLAGS)
      ctx->n_regs++;

    if (xed_operand_written(op)) {
      if (xed_reg_class(reg) == XED_REG_CLASS_FLAGS) {
        continue;
      }

      ctx->result_reg = reg;
    }
  }

  const xed_simple_flag_t *flags = xed_decoded_inst_get_rflags_info(&xedd);
  if (flags) {
    const xed_flag_set_t *flag_set =
        xed_simple_flag_get_written_flag_set(flags);
    ctx->flags_mask = xed_flag_set_mask(flag_set);
  } else {
    ctx->flags_mask = 0;
  }
}

bool is_instruction_supported(const xed_inst_t *inst) {
  xed_iclass_enum_t iclass = xed_inst_iclass(inst);
  xed_isa_set_enum_t isa_set = xed_inst_isa_set(inst);
  xed_category_enum_t category = xed_inst_category(inst);

  bool supported = true;

#define UNSUPPORTED_ATTRIBUTE(attr)                                            \
  supported = supported && !xed_inst_get_attribute(inst, attr)
#define UNSUPPORTED_ISA_SET(iset) supported = supported && isa_set != iset
#define UNSUPPORTED_ISA_SET_RANGE(start, end)                                  \
  supported = supported && !(start <= isa_set && isa_set <= end)
#define UNSUPPORTED_ICLASS(class) supported = supported && iclass != class
#define UNSUPPORTED_CATEGORY(cat) supported = supported && category != cat

#include "unsupported.h"

#undef UNSUPPORTED_ATTRIBUTE
#undef UNSUPPORTED_ISA_SET
#undef UNSUPPORTED_ISA_SET_RANGE
#undef UNSUPPORTED_ICLASS
#undef UNSUPPORTED_CATEGORY

  if (!supported)
    return false;

  unsigned int noperands = xed_inst_noperands(inst);

  if (noperands == 0)
    return false;

  unsigned int n_regs = 0;

  for (unsigned int j = 0; supported && j < noperands; j++) {
    const xed_operand_t *op = xed_inst_operand(inst, j);
    const xed_reg_enum_t reg = xed_operand_reg(op);

    xed_nonterminal_enum_t nonterminal = xed_operand_nonterminal_name(op);
    if (!((XED_NONTERMINAL_GPR16_B <= nonterminal &&
           nonterminal <= XED_NONTERMINAL_GPRZ_R) ||
          nonterminal == XED_NONTERMINAL_RFLAGS)) {
      supported = false;
      break;
    }

    if (XED_NONTERMINAL_GPR16_B <= nonterminal &&
        nonterminal <= XED_NONTERMINAL_GPRZ_R)
      n_regs++;
    else {
      // is rflags
      if (xed_operand_read(op))
        supported = false;
    }
  }

  return supported && 0 < n_regs && n_regs <= 3;
}

// Writes machine code to populate reg with val into buf
void populate_reg_code(xed_reg_enum_t reg, uint64_t val, uint8_t buf[10]) {
  assert(XED_REG_RAX <= reg && reg <= XED_REG_R15);

  uint8_t regnum = reg - XED_REG_RAX;

  buf[0] = regnum < 8 ? 0x48 : 0x49;
  buf[1] = 0xb8 + (regnum % 8);
  *(uint64_t *)(buf + 2) = val;
}

// Write machine code to move value in reg to rax
void mov_reg_to_rax_code(xed_reg_enum_t reg, uint8_t buf[3]) {
  assert(XED_REG_RAX <= reg && reg <= XED_REG_R15);

  uint8_t regnum = reg - XED_REG_RAX;

  buf[0] = regnum < 8 ? 0x48 : 0x4c;
  buf[1] = 0x89;
  buf[2] = 0xc0 | ((regnum % 8) << 3);
}

uint8_t mov_flags_to_rdx_code[] = {0x9c,  // pushf
                                   0x5a}; // pop rdx

uint8_t return_code[] = {0xc3}; // ret

struct testcase_return {
  uint64_t value;
  uint64_t flags;
};

void construct_testcase(struct inst_context *ctx, uint8_t inst_buf[15],
                        const xed_reg_enum_t regs[3],
                        const uint64_t reg_vals[3], uint8_t *buf, size_t *size,
                        xed_state_t *state) {
  size_t _size;
  if (!size)
    size = &_size;

  *size = 0;
  for (unsigned int i = 0; i < ctx->n_regs; i++) {
    populate_reg_code(regs[i], reg_vals[i], buf + *size);
    *size += 10;
  }

  memcpy(buf + *size, inst_buf, 15);
  *size += 15;

  if (ctx->result_reg != XED_REG_INVALID) {
    mov_reg_to_rax_code(xed_get_largest_enclosing_register(ctx->result_reg),
                        buf + *size);
    *size += 3;
  }

  memcpy(buf + *size, mov_flags_to_rdx_code, sizeof(mov_flags_to_rdx_code));
  *size += sizeof(mov_flags_to_rdx_code);

  memcpy(buf + *size, return_code, sizeof(return_code));
  *size += sizeof(return_code);
}

struct test_result {
  uint8_t inst_buf[15];
  xed_reg_enum_t value_reg;
  uint64_t value;
  uint64_t flags_mask;
  uint64_t flags;
};

// The registers to use in our test cases
// Note: These were specifically chosen as registers which are caller preserved
// under the SYSV ABI to keep from having to save them when we execute our test
// cases.
xed_reg_enum_t regs[][3] = {
    {XED_REG_RAX, XED_REG_RDI, XED_REG_RDX},
    // Using these registers allows us to test the decoding of r8-15
    {XED_REG_R9, XED_REG_R10, XED_REG_R11}};

// Just some integers that may have interesting behavior for some instructions
// We always test these, but randomly pick other test vectors
uint64_t interesting_values[] = {0, 1, UINT64_MAX};

#define N_INTERESTING_VALUES (sizeof(interesting_values) / sizeof(uint64_t))

// Number of testcases with 1 interesting value and the other random per
// interesting value and input
#define N_PARTIALLY_INTERESTING_CASES 25

// Number of testcases with completely random cases
#define N_RANDOM_CASES 50

struct __attribute__((packed)) inst_reg_record {
  uint8_t inst_buf[15];
  uint8_t regs[3];
  uint8_t value_reg;
  uint64_t flags_mask;
  uint64_t n_test_records;
};

struct __attribute__((packed)) inst_test_record {
  uint64_t reg_vals[3];
  uint64_t value;
  uint64_t flags;
};

void test_inst_with_values(
    const xed_inst_t *inst, struct inst_context *ctx, xed_reg_enum_t regs[3],
    __attribute__((sysv_abi)) struct testcase_return (*testcase)(),
    uint8_t inst_buf[15], struct inst_test_record *test_rec,
    xed_state_t *state) {
  printf("Vals %lx %lx %lx\n", test_rec->reg_vals[0], test_rec->reg_vals[1],
         test_rec->reg_vals[2]);

  size_t size;
  construct_testcase(ctx, inst_buf, regs, test_rec->reg_vals,
                     (uint8_t *)testcase, &size, state);

  printf("Testcase: ");
  print_hex((uint8_t *)testcase, size);
  printf("\n");

  struct testcase_return testret = testcase();
  test_rec->value = testret.value;
  test_rec->flags = testret.flags & ctx->flags_mask;

  printf("Got Val: %lx; Flags: %lx\n", test_rec->value, test_rec->flags);
}

// This is not great
uint64_t rand64() {
  uint64_t r = 0;
  for (uint8_t i = 0; i < 64; i += 1) {
    r = (r << 1) | (rand() & 1);
  }
  return r;
}

void test_inst_regs(
    const xed_inst_t *inst, xed_reg_enum_t regs[3],
    __attribute__((sysv_abi)) struct testcase_return (*testcase)(),
    FILE *stream, xed_state_t *state) {
  struct inst_reg_record inst_reg;

  bool fail = encode_inst(inst, inst_reg.inst_buf, regs, state);
  if (fail) {
    return;
  }

  struct inst_context ctx;
  get_inst_context(inst_reg.inst_buf, &ctx, state);

  for (uint8_t i = 0; i < 3; i++)
    inst_reg.regs[i] = regs[i] - XED_REG_RAX;
  inst_reg.value_reg = ctx.result_reg - XED_REG_RAX;
  inst_reg.flags_mask = ctx.flags_mask;

  inst_reg.n_test_records = 1;
  // Compute number of interesting testcases
  for (uint8_t i = 0; i < ctx.n_regs; i++)
    inst_reg.n_test_records *= N_INTERESTING_VALUES;
  inst_reg.n_test_records +=
      ctx.n_regs * N_INTERESTING_VALUES * N_PARTIALLY_INTERESTING_CASES;
  inst_reg.n_test_records += N_RANDOM_CASES;

  fwrite(&inst_reg, sizeof(inst_reg), 1, stream);

  // Run interesting testcases
  struct inst_test_record test_rec = {};
  uint8_t val_idxs[3] = {0, 0, 0};
  while (val_idxs[0] != N_INTERESTING_VALUES) {
    for (uint8_t i = 0; i < ctx.n_regs; i++) {
      test_rec.reg_vals[i] = interesting_values[val_idxs[i]];
    }

    test_inst_with_values(inst, &ctx, regs, testcase, inst_reg.inst_buf,
                          &test_rec, state);
    fwrite(&test_rec, sizeof(test_rec), 1, stream);

    for (uint8_t i = ctx.n_regs - 1; i >= 0; i--) {
      val_idxs[i]++;

      if (val_idxs[i] != N_INTERESTING_VALUES)
        break;

      if (i != 0)
        val_idxs[i] %= N_INTERESTING_VALUES;
    }
  }

  // Run partially interesting cases
  for (uint8_t i = 0; i < ctx.n_regs; i++) {
    for (uint8_t j = 0; j < N_INTERESTING_VALUES; j++) {
      test_rec.reg_vals[i] = interesting_values[j];

      for (uint8_t count = 0; count < N_PARTIALLY_INTERESTING_CASES; count++) {
        for (uint8_t k = 0; k < ctx.n_regs; k++) {
          if (i == k)
            continue;
          test_rec.reg_vals[k] = rand64();
        }

        test_inst_with_values(inst, &ctx, regs, testcase, inst_reg.inst_buf,
                              &test_rec, state);
        fwrite(&test_rec, sizeof(test_rec), 1, stream);
      }
    }
  }

  // Run random cases
  for (uint8_t i = 0; i < N_RANDOM_CASES; i++) {
    for (uint8_t j = 0; j < ctx.n_regs; j++) {
      test_rec.reg_vals[j] = rand64();
    }

    test_inst_with_values(inst, &ctx, regs, testcase, inst_reg.inst_buf,
                          &test_rec, state);
    fwrite(&test_rec, sizeof(test_rec), 1, stream);
  }
}

void test_inst(const xed_inst_t *inst,
               __attribute__((sysv_abi)) struct testcase_return (*testcase)(),
               FILE *stream, xed_state_t *state) {
  for (uint8_t i = 0; i < sizeof(regs) / (3 * sizeof(xed_reg_enum_t)); i++)
    test_inst_regs(inst, regs[i], testcase, stream, state);
}

int main() {
  srand(1234);
  xed_tables_init();

  // Initialize the XED state machine for decoding
  xed_state_t state;
  xed_state_zero(&state);

  // Initialize for 64-bit mode
  xed_state_init2(&state, XED_MACHINE_MODE_LONG_64, XED_ADDRESS_WIDTH_64b);

  printf("XED initialized successfully!\n");

  void *testcase = mmap(NULL, 0x1000, PROT_EXEC | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  FILE *stream = fopen("dandelion.out", "w");

  const xed_inst_t *table = xed_inst_table_base();
  uint64_t vals[] = {10, 20, 30};

  for (size_t i = 0; i < XED_MAX_INST_TABLE_NODES; i++) {
    const xed_inst_t *inst = &table[i];
    xed_iclass_enum_t iclass = xed_inst_iclass(&table[i]);
    if (!is_instruction_supported(inst))
      continue;

    printf("\nTesting %s\n", xed_iclass_enum_t2str(iclass));
    printf("ISA_SET: %s\n", xed_isa_set_enum_t2str(xed_inst_isa_set(inst)));

    test_inst(inst, testcase, stream, &state);
  }

  fclose(stream);

  return 0;
}
