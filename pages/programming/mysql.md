# MySQL常用命令

```sql
// 创建表
CREATE TALBE student (
number INT(10) NOT NULL DEFAULT '',
name VARCHAR(255) NOT NULL DEFAULT '',
birthday date
);

// 查看表
show tables;
desc student;

// 删除表
DROP TABLE student;
DROP TABLE IF EXISTS student;

// 修改字段
ALTER TABLE student CHANGE number inumber INT(12) NULL;
ALTER TABLE studnet MODIFY number INT(12) NULL; // `modiy` will keep the name unchanged
ALTER TABLE student ADD birthday DATE;
ALTER TABLE student DROP birthday;

// 插入数据
INSERT INTO student values('2014100432', 'isaac');
INSERT INTO student (number, name) values('2013105645', 'jenny'),('2013100475', 'jack');
```
