
## Functional Testing

### **1. Testing SOP (Standard Operating Procedure)**

#### **Directory & Naming Conventions**

* **Test Directory:** All test source files reside in the `test/` directory.
* **File Naming:** Test files follow the pattern `test_<module_name>.f90` (e.g., `test_board.f90`, `test_fen.f90`).
* **fpm Manifest (`fpm.toml`):** Every test program must be declared in `fpm.toml`:
```toml
[[test]]
name = "test_board"
source-dir = "test"
main = "test_board.f90"

```

#### **Execution Workflows**

* **Run All Tests (Standard Build):**
```bash
fpm test

```

* **Run All Tests with Debug Profiling & Sanity Flags:**
```bash
fpm test --profile debug --flag "-std=f2018 -Wall -Wextra -Werror -fcheck=all -g -fbacktrace -cpp -DDEBUG"

```

* **Run a Specific Test Program:**
```bash
fpm test test_board

```

* **Debug a Failing Test with `lldb` (macOS):**
```bash
sudo lldb $(find build -name "test_board" -type f | head -n 1)

```

---

### **2. Fortran Test Suite Template/Schema**

When creating a new module test (e.g., `test_foo.f90`), use the standard schema leveraging `TestUtilsM`:

```fortran
program test_foo
  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  ! use FooM, only: foo_function
  implicit none

  call begin_suite("test_foo")

  call test_basic_functionality()
  call test_edge_case()

contains

  subroutine test_basic_functionality()
    call begin_test("basic functionality")

    ! Assert condition
    if (1 + 1 == 2) then
      call pass()
    else
      call fail("1 + 1 did not equal 2")
    end if
  end subroutine test_basic_functionality

  subroutine test_edge_case()
    call begin_test("edge case handling")

    ! Assert condition with detail
    if (.true.) then
      call pass("edge condition satisfied")
    else
      call fail("unexpected edge condition failure")
    end if
  end subroutine test_edge_case

end program test_foo

```

---

### **3. Document Storage Strategy**

Save this specification inside your repository at **`docs/testing.md`**.

```
.
├── docs/
│   ├── adr/
│   │   └── 0004-clockm-integration.md
│   └── testing.md
├── src/
├── test/
│   ├── TestUtilsM.f90
│   ├── test_base.f90
│   └── test_foo.f90
└── fpm.toml

```

