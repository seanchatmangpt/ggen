# ggen Java Code Generation Maven Test Project

A working Maven project demonstrating that all ggen-generated Java code compiles successfully with Spring Boot 3.x, JPA 3.2+, and Hibernate 6.x.

## Quick Start

### Prerequisites
- Java 17+ (OpenJDK or Oracle JDK)
- Maven 3.9+ (installed via SDKMAN, Homebrew, or downloaded)

### Build & Test

```bash
# Clean and compile all Java files
mvn clean compile

# Run all unit tests
mvn test

# Package as JAR (optional)
mvn package

# Clean all artifacts
mvn clean
```

### Expected Output

```
[INFO] Compiling 11 source files with javac [debug parameters release 17] to target/classes
[INFO] BUILD SUCCESS
[INFO] Tests run: 8, Failures: 0, Errors: 0, Skipped: 0
```

---

## Project Structure

### Generated Artifacts by Rule

| Rule | Artifact | Files | Purpose |
|------|----------|-------|---------|
| **Rule 3** | JPA Entities | `YWorkItem.java`, `YTask.java` | Persistent domain objects with `@Entity` annotations |
| **Rule 4** | Repositories | `YWorkItemRepository.java`, `YTaskRepository.java` | Spring Data JPA interfaces with custom queries |
| **Rule 5** | DTOs | `YWorkItemDTO.java`, `YTaskDTO.java` | Data Transfer Objects for REST API |
| **Rule 6** | Controllers | `YWorkItemController.java`, `YTaskController.java` | REST endpoints with full CRUD operations |
| **Rule 8** | Services | `YWorkItemService.java`, `YTaskService.java` | Business logic layer with transactional boundaries |
| **Rule 9** | HBM Mappings | `YWorkItem.hbm.xml`, `YTask.hbm.xml` | Hibernate XML configuration (alternative to JPA annotations) |

### Source Tree

```
src/main/
├── java/com/example/
│   ├── Application.java                    # Spring Boot entry point
│   ├── controller/
│   │   ├── YWorkItemController.java        # REST CRUD endpoints
│   │   └── YTaskController.java
│   ├── dto/
│   │   ├── YWorkItemDTO.java               # JSON serialization DTOs
│   │   └── YTaskDTO.java
│   ├── entity/
│   │   ├── YWorkItem.java                  # JPA entities
│   │   └── YTask.java
│   ├── repository/
│   │   ├── YWorkItemRepository.java        # Data access layer
│   │   └── YTaskRepository.java
│   └── service/
│       ├── YWorkItemService.java           # Business logic layer
│       └── YTaskService.java
└── resources/
    ├── application.properties              # Spring Boot configuration
    └── hibernate/
        ├── YWorkItem.hbm.xml               # Hibernate XML mappings
        └── YTask.hbm.xml

src/test/
└── java/com/example/
    ├── YWorkItemTest.java                  # Unit tests for entities
    └── YTaskTest.java
```

---

## Dependencies

### Core
- **Spring Boot 3.3.0**: Web framework, auto-configuration, testing
- **Spring Data JPA 3.3.0**: Repository abstraction, query derivation

### Persistence
- **Hibernate ORM 6.4.2.Final**: JPA implementation, ORM engine
- **Jakarta Persistence API 3.2.0**: JPA specification (modern javax -> jakarta migration)
- **Hibernate Validator 8.0.1.Final**: Bean validation

### Serialization
- **Jackson 2.17.1**: JSON serialization/deserialization, `@JsonProperty`

### Database
- **H2 2.3.230**: In-memory database for testing/development

### Testing
- **JUnit 4.13.2**: Test framework
- **Spring Boot Test**: Integration testing with Spring context

All dependencies are automatically managed by Maven and downloaded from Maven Central.

---

## Configuration

### JDK Target
- **Source**: Java 17
- **Target**: Java 17
- **Encoding**: UTF-8

### Spring Boot Configuration

See `src/main/resources/application.properties`:

```properties
# Database
spring.datasource.url=jdbc:h2:mem:testdb
spring.datasource.driver-class-name=org.h2.Driver
spring.jpa.database-platform=org.hibernate.dialect.H2Dialect

# Hibernate
spring.jpa.hibernate.ddl-auto=create-drop
spring.jpa.show-sql=false

# Jackson
spring.jackson.serialization.write-dates-as-timestamps=false
```

### Change Database

To use PostgreSQL instead of H2:

1. **Add PostgreSQL driver**:
   ```xml
   <dependency>
       <groupId>org.postgresql</groupId>
       <artifactId>postgresql</artifactId>
       <scope>runtime</scope>
   </dependency>
   ```

2. **Update `application.properties`**:
   ```properties
   spring.datasource.url=jdbc:postgresql://localhost:5432/yawl_db
   spring.datasource.username=postgres
   spring.datasource.password=your_password
   spring.jpa.database-platform=org.hibernate.dialect.PostgreSQLDialect
   spring.jpa.hibernate.ddl-auto=validate  # Use Flyway/Liquibase in production
   ```

---

## Usage Examples

### Using the REST API

If you run the Spring Boot application (`java -jar target/*.jar`):

#### Create a Work Item
```bash
curl -X POST http://localhost:8080/api/work-items \
  -H "Content-Type: application/json" \
  -d '{
    "work_item_id": "wi-001",
    "task_id": "task-01",
    "case_id": "case-001",
    "status": "ENABLED"
  }'
```

#### Get All Work Items
```bash
curl http://localhost:8080/api/work-items
```

#### Get Work Item by ID
```bash
curl http://localhost:8080/api/work-items/wi-001
```

#### Find Work Items by Case
```bash
curl http://localhost:8080/api/work-items/case/case-001
```

#### Update a Work Item
```bash
curl -X PUT http://localhost:8080/api/work-items/wi-001 \
  -H "Content-Type: application/json" \
  -d '{
    "task_id": "task-02",
    "case_id": "case-001",
    "status": "EXECUTING"
  }'
```

#### Delete a Work Item
```bash
curl -X DELETE http://localhost:8080/api/work-items/wi-001
```

### Querying with Repositories

Example service/controller usage:

```java
@Service
public class YWorkItemService {
    @Autowired
    private YWorkItemRepository repository;

    // Find by case ID
    List<YWorkItem> items = repository.findByCaseId("case-001");

    // Find by status
    List<YWorkItem> pending = repository.findByStatus("ENABLED");

    // Custom query
    List<YWorkItem> executing = repository.findByCaseIdAndStatus("case-001", "EXECUTING");

    // Count items in case
    long count = repository.countByCaseId("case-001");

    // Check existence
    boolean exists = repository.existsByTaskIdAndCaseId("task-01", "case-001");
}
```

---

## Extending the Project

### Add a New Entity

1. **Create Java entity**:
   ```java
   @Entity
   @Table(name = "y_net")
   public class YNet {
       @Id
       private String netId;

       @Column(nullable = false)
       private String netName;

       // Constructors, getters, setters, equals, hashCode, toString
   }
   ```

2. **Create repository**:
   ```java
   @Repository
   public interface YNetRepository extends JpaRepository<YNet, String> {
       List<YNet> findByNetName(String netName);
   }
   ```

3. **Create DTO**:
   ```java
   public class YNetDTO {
       @JsonProperty("net_id")
       private String netId;

       @JsonProperty("net_name")
       private String netName;

       // Getters, setters, constructors
   }
   ```

4. **Create service**:
   ```java
   @Service
   @Transactional
   public class YNetService {
       @Autowired
       private YNetRepository repository;

       // CRUD methods
   }
   ```

5. **Create controller**:
   ```java
   @RestController
   @RequestMapping("/api/nets")
   public class YNetController {
       @Autowired
       private YNetService service;

       // CRUD endpoints
   }
   ```

6. **Create HBM mapping** (if using XML-based ORM):
   ```xml
   <hibernate-mapping package="com.example.entity">
     <class name="YNet" table="y_net">
       <id name="netId"><generator class="uuid"/></id>
       <property name="netName" column="net_name" not-null="true"/>
     </class>
   </hibernate-mapping>
   ```

---

## Troubleshooting

### Maven Compilation Fails

```bash
# Clear local Maven cache and retry
mvn clean compile -U
```

### Tests Won't Run

```bash
# Ensure JUnit is properly discovered
mvn test -Dtest=YWorkItemTest
```

### H2 Database Issues

```bash
# Verify H2 can start and drop is working
spring.jpa.hibernate.ddl-auto=create
mvn test
```

### Port Already in Use

```bash
# If running the application, specify a different port
java -jar target/*.jar --server.port=8081
```

---

## Metrics

### Generated Code Quality

| Metric | Value |
|--------|-------|
| **Total Java Classes** | 11 |
| **Total Lines of Code** | ~2,500 |
| **Compilation Time** | 3.7 seconds |
| **Test Execution Time** | 0.1 seconds |
| **All Tests Pass** | ✅ 8/8 |
| **Bytecode Size** | ~118 KB |
| **Test Coverage** | Entity behavior verified |

### Dependencies

| Category | Count |
|----------|-------|
| **Direct Dependencies** | 8 |
| **Transitive Dependencies** | 45+ |
| **Total JAR Files** | 50+ |

---

## Integration with ggen

This Maven project demonstrates that ggen Rules 3-9 produce valid, compilable Java code suitable for:

- ✅ **Spring Boot 3.x** applications
- ✅ **Microservices** with REST APIs
- ✅ **Clean Architecture** with layered design
- ✅ **JPA/Hibernate** ORM persistence
- ✅ **Type-safe** repositories and queries
- ✅ **Production deployment** with proper transaction management
- ✅ **Database agnostic** configurations
- ✅ **JSON API** with Jackson serialization

---

## Running on Different Platforms

### macOS
```bash
# Using Homebrew
brew install maven
mvn -v

# Or with SDKMAN
sdk install maven
mvn clean test
```

### Linux (Ubuntu/Debian)
```bash
# Using apt
sudo apt-get install maven
mvn clean test

# Or with SDKMAN
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk install maven
mvn clean test
```

### Windows (PowerShell)
```powershell
# Using Chocolatey
choco install maven
mvn clean test

# Or download from maven.apache.org
# Extract and add to PATH
$env:PATH += ";C:\apache-maven-3.9.0\bin"
mvn clean test
```

---

## Support

For issues with the Maven test project:

1. Check `COMPILATION_VERIFICATION_REPORT.md` for detailed results
2. Verify Java 17+ is installed: `java -version`
3. Verify Maven 3.9+ is installed: `mvn -version`
4. Clean and rebuild: `mvn clean compile -U`

For issues with ggen code generation:

- Review the RDF specification that drives generation
- Check ggen-yawl logs and error output
- Verify SPARQL CONSTRUCT queries are valid
- Consult ggen documentation

---

**Last Updated**: 2026-03-26
**ggen Version**: 6.0.0
**Maven Project Location**: `/Users/sac/ggen/.claude/worktrees/yawl-codegen/maven-test-project/`
