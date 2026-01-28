# TypeScript 语法指南与开发手册

## 1. TypeScript 基础语法

### 1.1 类型系统

#### 1.1.1 基本类型
```typescript
// 布尔值
let isDone: boolean = false;

// 数字
let decimal: number = 6;
let hex: number = 0xf00d;
let binary: number = 0b1010;
let octal: number = 0o744;

// 字符串
let color: string = "blue";
let fullName: string = `Bob Bobbington`;
let age: number = 37;
let sentence: string = `Hello, my name is ${fullName}. I'll be ${age + 1} years old next month.`;

// 数组
let list: number[] = [1, 2, 3];
let list: Array<number> = [1, 2, 3];

// 元组
let x: [string, number];
x = ["hello", 10]; // OK

// 枚举
enum Color {
  Red,
  Green,
  Blue,
}
let c: Color = Color.Green;

// Any
let notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // okay, definitely a boolean

// Void
function warnUser(): void {
  console.log("This is my warning message");
}

// Null 和 Undefined
let u: undefined = undefined;
let n: null = null;

// Never
function error(message: string): never {
  throw new Error(message);
}

// Object
function create(o: object | null): void {}
```

#### 1.1.2 类型断言
```typescript
// 尖括号语法
let someValue: any = "this is a string";
let strLength: number = (<string>someValue).length;

// as 语法
let someValue: any = "this is a string";
let strLength: number = (someValue as string).length;
```

### 1.2 接口

#### 1.2.1 基本接口
```typescript
interface LabelledValue {
  label: string;
}

function printLabel(labelledObj: LabelledValue) {
  console.log(labelledObj.label);
}

let myObj = { size: 10, label: "Size 10 Object" };
printLabel(myObj);
```

#### 1.2.2 可选属性
```typescript
interface SquareConfig {
  color?: string;
  width?: number;
}

function createSquare(config: SquareConfig): { color: string; area: number } {
  let newSquare = { color: "white", area: 100 };
  if (config.color) {
    newSquare.color = config.color;
  }
  if (config.width) {
    newSquare.area = config.width * config.width;
  }
  return newSquare;
}
```

#### 1.2.3 只读属性
```typescript
interface Point {
  readonly x: number;
  readonly y: number;
}

let p1: Point = { x: 10, y: 20 };
p1.x = 5; // 错误！
```

### 1.3 函数

#### 1.3.1 函数类型
```typescript
function add(x: number, y: number): number {
  return x + y;
}

let myAdd: (x: number, y: number) => number = function (x: number, y: number): number {
  return x + y;
};
```

#### 1.3.2 可选参数和默认参数
```typescript
function buildName(firstName: string, lastName?: string) {
  if (lastName) return `${firstName} ${lastName}`;
  else return firstName;
}

function buildName(firstName: string, lastName = "Smith") {
  return `${firstName} ${lastName}`;
}
```

#### 1.3.3 剩余参数
```typescript
function buildName(firstName: string, ...restOfName: string[]) {
  return `${firstName} ${restOfName.join(" ")}`;
}
```

### 1.4 类

#### 1.4.1 基本类
```typescript
class Greeter {
  greeting: string;
  constructor(message: string) {
    this.greeting = message;
  }
  greet() {
    return `Hello, ${this.greeting}`;
  }
}

let greeter = new Greeter("world");
```

#### 1.4.2 继承
```typescript
class Animal {
  move(distanceInMeters: number = 0) {
    console.log(`Animal moved ${distanceInMeters}m.`);
  }
}

class Dog extends Animal {
  bark() {
    console.log("Woof! Woof!");
  }
}

const dog = new Dog();
dog.bark();
dog.move(10);
dog.bark();
```

#### 1.4.3 公共、私有与受保护的修饰符
```typescript
class Animal {
  public name: string;
  private age: number;
  protected species: string;

  constructor(name: string, age: number, species: string) {
    this.name = name;
    this.age = age;
    this.species = species;
  }
}
```

### 1.5 泛型

#### 1.5.1 基本泛型
```typescript
function identity<T>(arg: T): T {
  return arg;
}

let output = identity<string>("myString");
let output = identity("myString"); // 类型推论
```

#### 1.5.2 泛型接口
```typescript
interface GenericIdentityFn<T> {
  (arg: T): T;
}

function identity<T>(arg: T): T {
  return arg;
}

let myIdentity: GenericIdentityFn<number> = identity;
```

#### 1.5.3 泛型类
```typescript
class GenericNumber<T> {
  zeroValue: T;
  add: (x: T, y: T) => T;
}

let myGenericNumber = new GenericNumber<number>();
myGenericNumber.zeroValue = 0;
myGenericNumber.add = function (x, y) { return x + y; };
```

### 1.6 高级类型

#### 1.6.1 交叉类型
```typescript
type Combined = { a: number } & { b: string };
let obj: Combined = { a: 1, b: "hello" };
```

#### 1.6.2 联合类型
```typescript
type Union = string | number;
let value: Union = "hello";
value = 42;
```

#### 1.6.3 类型守卫
```typescript
function isFish(pet: Fish | Bird): pet is Fish {
  return (pet as Fish).swim !== undefined;
}

if (isFish(pet)) {
  pet.swim();
} else {
  pet.fly();
}
```

#### 1.6.4 类型别名
```typescript
type Name = string;
type NameResolver = () => string;
type NameOrResolver = Name | NameResolver;
```

#### 1.6.5 字符串字面量类型
```typescript
type Easing = "ease-in" | "ease-out" | "ease-in-out";
```

## 2. 模块化开发

### 2.1 TypeScript 模块系统

#### 2.1.1 ES 模块
```typescript
// 导出
export const PI = 3.14;
export function calculateArea(radius: number): number {
  return PI * radius * radius;
}
export class Circle {
  constructor(private radius: number) {}
  getArea() { return calculateArea(this.radius); }
}

// 导入
import { PI, calculateArea, Circle } from "./math";
import * as MathUtils from "./math";
```

#### 2.1.2 默认导出
```typescript
// 导出
export default function add(x: number, y: number): number {
  return x + y;
}

// 导入
import add from "./math";
```

### 2.2 后端模块化（NestJS）

#### 2.2.1 模块定义
```typescript
import { Module } from '@nestjs/common';
import { AuthService } from './auth.service';
import { AuthController } from './auth.controller';
import { UserEntity } from './entities/user.entity';
import { TypeOrmModule } from '@nestjs/typeorm';

@Module({
  imports: [TypeOrmModule.forFeature([UserEntity])],
  controllers: [AuthController],
  providers: [AuthService],
  exports: [AuthService],
})
export class AuthModule {}
```

#### 2.2.2 模块注入
```typescript
import { Module } from '@nestjs/common';
import { AuthModule } from './modules/auth/auth.module';
import { AutomationModule } from './modules/automation/automation.module';

@Module({
  imports: [AuthModule, AutomationModule],
})
export class AppModule {}
```

### 2.3 前端模块化（React）

#### 2.3.1 组件模块化
```typescript
// components/Button/Button.tsx
import React from 'react';

interface ButtonProps {
  onClick: () => void;
  children: React.ReactNode;
  variant?: 'primary' | 'secondary';
}

export const Button: React.FC<ButtonProps> = ({ onClick, children, variant = 'primary' }) => {
  return (
    <button 
      onClick={onClick}
      className={`button button-${variant}`}
    >
      {children}
    </button>
  );
};

// 使用
import { Button } from './components/Button/Button';
```

#### 2.3.2 状态模块化
```typescript
// store/authSlice.ts
import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface AuthState {
  user: { id: number; username: string } | null;
  token: string | null;
  loading: boolean;
}

const initialState: AuthState = {
  user: null,
  token: null,
  loading: false,
};

export const authSlice = createSlice({
  name: 'auth',
  initialState,
  reducers: {
    setUser: (state, action: PayloadAction<{ user: AuthState['user']; token: AuthState['token'] }>) => {
      state.user = action.payload.user;
      state.token = action.payload.token;
    },
    logout: (state) => {
      state.user = null;
      state.token = null;
    },
  },
});

export const { setUser, logout } = authSlice.actions;
export default authSlice.reducer;
```

## 3. 函数式开发

### 3.1 核心概念

#### 3.1.1 纯函数
```typescript
// 纯函数：相同输入总是产生相同输出，无副作用
function add(a: number, b: number): number {
  return a + b;
}

// 不纯函数：依赖外部状态，有副作用
let counter = 0;
function increment(): number {
  counter++;
  return counter;
}
```

#### 3.1.2 不可变性
```typescript
// 可变操作（避免）
const obj = { a: 1 };
obj.a = 2; // 修改原始对象

// 不可变操作（推荐）
const obj = { a: 1 };
const newObj = { ...obj, a: 2 }; // 创建新对象

// 数组不可变操作
const arr = [1, 2, 3];
const newArr = [...arr, 4]; // 添加元素
const filteredArr = arr.filter(x => x > 1); // 过滤元素
const mappedArr = arr.map(x => x * 2); // 映射元素
```

#### 3.1.3 函数组合
```typescript
// 函数组合：将多个小函数组合成一个大函数
function compose<A, B, C>(f: (b: B) => C, g: (a: A) => B): (a: A) => C {
  return (a: A) => f(g(a));
}

function addOne(x: number): number {
  return x + 1;
}

function multiplyByTwo(x: number): number {
  return x * 2;
}

const addOneThenMultiplyByTwo = compose(multiplyByTwo, addOne);
addOneThenMultiplyByTwo(5); // 12
```

#### 3.1.4 高阶函数
```typescript
// 高阶函数：接受函数作为参数或返回函数
function withLogging<T extends (...args: any[]) => any>(fn: T): (...args: Parameters<T>) => ReturnType<T> {
  return (...args: Parameters<T>) => {
    console.log(`Calling ${fn.name} with`, args);
    const result = fn(...args);
    console.log(`Result:`, result);
    return result;
  };
}

const loggedAdd = withLogging((a: number, b: number) => a + b);
loggedAdd(2, 3); // 5
```

### 3.2 函数式工具库

#### 3.2.1 Ramda.js
```typescript
import * as R from 'ramda';

// 函数组合
const addOneThenDouble = R.compose(R.multiply(2), R.add(1));
addOneThenDouble(5); // 12

// 柯里化
const add = R.curry((a: number, b: number) => a + b);
const add5 = add(5);
add5(3); // 8

// 管道
const processData = R.pipe(
  R.filter((x: number) => x > 0),
  R.map((x: number) => x * 2),
  R.sum
);
processData([1, -2, 3, 4]); // 16
```

#### 3.2.2 fp-ts
```typescript
import { Option, some, none } from 'fp-ts/Option';
import { Either, left, right } from 'fp-ts/Either';
import { pipe } from 'fp-ts/function';

// Option 类型
function safeDivide(a: number, b: number): Option<number> {
  return b === 0 ? none : some(a / b);
}

// Either 类型
function parseJSON(s: string): Either<Error, unknown> {
  try {
    return right(JSON.parse(s));
  } catch (e) {
    return left(e instanceof Error ? e : new Error('Invalid JSON'));
  }
}

// 管道操作
const result = pipe(
  some(5),
  Option.map(x => x * 2),
  Option.getOrElse(() => 0)
);
```

### 3.3 函数式错误处理

#### 3.3.1 Result 类型
```typescript
type Result<T, E> = {
  type: 'Ok';
  value: T;
} | {
  type: 'Err';
  error: E;
};

function ok<T, E>(value: T): Result<T, E> {
  return { type: 'Ok', value };
}

function err<T, E>(error: E): Result<T, E> {
  return { type: 'Err', error };
}

function divide(a: number, b: number): Result<number, string> {
  if (b === 0) {
    return err('Division by zero');
  }
  return ok(a / b);
}

const result = divide(10, 2);
if (result.type === 'Ok') {
  console.log(`Result: ${result.value}`);
} else {
  console.error(`Error: ${result.error}`);
}
```

### 3.4 函数式实践

#### 3.4.1 后端服务层
```typescript
// 纯函数处理业务逻辑
export function calculateExecutionTime(startTime: Date, endTime: Date): number {
  return endTime.getTime() - startTime.getTime();
}

// 函数式数据转换
export function formatScriptOutput(output: string): string[] {
  return output
    .split('\n')
    .filter(line => line.trim() !== '')
    .map(line => line.trim());
}

// 高阶函数处理依赖注入
export function createAuthService(userRepository: UserRepository) {
  return {
    login: async (username: string, password: string) => {
      const user = await userRepository.findByUsername(username);
      if (!user) {
        return err('User not found');
      }
      // 验证密码等逻辑
      return ok({ user, token: generateToken(user) });
    },
  };
}
```

#### 3.4.2 前端组件
```typescript
// 纯函数组件
interface UserCardProps {
  user: {
    id: number;
    name: string;
    email: string;
  };
}

export const UserCard: React.FC<UserCardProps> = ({ user }) => {
  return (
    <div className="user-card">
      <h3>{user.name}</h3>
      <p>{user.email}</p>
    </div>
  );
};

// 函数式状态管理
import { useReducer } from 'react';

interface State {
  count: number;
}

type Action = { type: 'INCREMENT' } | { type: 'DECREMENT' } | { type: 'RESET' };

const initialState: State = { count: 0 };

function counterReducer(state: State, action: Action): State {
  switch (action.type) {
    case 'INCREMENT':
      return { count: state.count + 1 };
    case 'DECREMENT':
      return { count: state.count - 1 };
    case 'RESET':
      return initialState;
    default:
      return state;
  }
}

export const Counter: React.FC = () => {
  const [state, dispatch] = useReducer(counterReducer, initialState);
  
  return (
    <div>
      <p>Count: {state.count}</p>
      <button onClick={() => dispatch({ type: 'INCREMENT' })}>Increment</button>
      <button onClick={() => dispatch({ type: 'DECREMENT' })}>Decrement</button>
      <button onClick={() => dispatch({ type: 'RESET' })}>Reset</button>
    </div>
  );
};
```

## 4. 技术栈介绍

### 4.1 后端技术

#### 4.1.1 NestJS
- **核心特性**：模块化架构、依赖注入、装饰器语法、中间件支持
- **使用示例**：
  ```typescript
  import { Controller, Get, Post, Body } from '@nestjs/common';
  import { AuthService } from './auth.service';
  import { LoginDto } from './dto/login.dto';

  @Controller('auth')
  export class AuthController {
    constructor(private authService: AuthService) {}

    @Post('login')
    async login(@Body() loginDto: LoginDto) {
      return this.authService.login(loginDto);
    }
  }
  ```

#### 4.1.2 PostgreSQL
- **核心特性**：强大的关系型数据库、支持复杂查询、事务、JSON 数据类型
- **使用示例**：
  ```typescript
  import { Entity, PrimaryGeneratedColumn, Column } from 'typeorm';

  @Entity('users')
  export class UserEntity {
    @PrimaryGeneratedColumn()
    id: number;

    @Column({ unique: true })
    username: string;

    @Column()
    password: string;
  }
  ```

#### 4.1.3 Redis
- **核心特性**：内存数据库、键值存储、支持多种数据结构、缓存功能
- **使用示例**：
  ```typescript
  import { createClient } from 'redis';

  const redisClient = createClient({
    url: 'redis://localhost:6379',
  });

  async function cacheData(key: string, value: any, ttl: number) {
    await redisClient.set(key, JSON.stringify(value), {
      EX: ttl,
    });
  }

  async function getData(key: string) {
    const data = await redisClient.get(key);
    return data ? JSON.parse(data) : null;
  }
  ```

#### 4.1.4 RabbitMQ
- **核心特性**：消息队列、可靠消息传递、支持多种消息模式
- **使用示例**：
  ```typescript
  import * as amqp from 'amqplib';

  async function sendMessage() {
    const connection = await amqp.connect('amqp://localhost');
    const channel = await connection.createChannel();
    
    const queue = 'tasks';
    await channel.assertQueue(queue, { durable: true });
    
    const message = 'Hello World!';
    channel.sendToQueue(queue, Buffer.from(message), {
      persistent: true,
    });
    
    console.log(`Sent: ${message}`);
    setTimeout(() => {
      connection.close();
    }, 500);
  }
  ```

### 4.2 前端技术

#### 4.2.1 React
- **核心特性**：组件化、虚拟 DOM、单向数据流、Hooks
- **使用示例**：
  ```typescript
  import React, { useState, useEffect } from 'react';

  export const Counter: React.FC = () => {
    const [count, setCount] = useState(0);

    useEffect(() => {
      document.title = `Count: ${count}`;
    }, [count]);

    return (
      <div>
        <p>You clicked {count} times</p>
        <button onClick={() => setCount(count + 1)}>
          Click me
        </button>
      </div>
    );
  };
  ```

#### 4.2.2 Ant Design
- **核心特性**：企业级 UI 组件库、设计规范统一、响应式布局
- **使用示例**：
  ```typescript
  import React from 'react';
  import { Button, Table, Form, Input } from 'antd';

  export const UserTable: React.FC = () => {
    const columns = [
      { title: 'Name', dataIndex: 'name', key: 'name' },
      { title: 'Age', dataIndex: 'age', key: 'age' },
      { title: 'Address', dataIndex: 'address', key: 'address' },
      { 
        title: 'Action', 
        key: 'action',
        render: () => <Button type="primary">Edit</Button>
      },
    ];

    const data = [
      { key: '1', name: 'John Brown', age: 32, address: 'New York' },
      { key: '2', name: 'Jim Green', age: 42, address: 'London' },
    ];

    return <Table columns={columns} dataSource={data} />;
  };
  ```

#### 4.2.3 React Query
- **核心特性**：数据请求管理、缓存、自动重试、分页
- **使用示例**：
  ```typescript
  import { useQuery, useMutation } from 'react-query';
  import axios from 'axios';

  function fetchUsers() {
    return axios.get('/api/users').then(res => res.data);
  }

  export const UsersList: React.FC = () => {
    const { data, isLoading, error } = useQuery('users', fetchUsers);

    if (isLoading) return <div>Loading...</div>;
    if (error) return <div>Error: {error.message}</div>;

    return (
      <ul>
        {data.map(user => (
          <li key={user.id}>{user.name}</li>
        ))}
      </ul>
    );
  };
  ```

#### 4.2.4 Redux Toolkit
- **核心特性**：简化 Redux 使用、内置 immer、标准化 async logic
- **使用示例**：
  ```typescript
  import { createSlice, configureStore } from '@reduxjs/toolkit';

  const counterSlice = createSlice({
    name: 'counter',
    initialState: { value: 0 },
    reducers: {
      increment: state => {
        state.value += 1;
      },
      decrement: state => {
        state.value -= 1;
      },
    },
  });

  export const { increment, decrement } = counterSlice.actions;

  const store = configureStore({
    reducer: counterSlice.reducer,
  });

  // 使用
  store.dispatch(increment());
  console.log(store.getState()); // { value: 1 }
  ```

### 4.3 开发工具

#### 4.3.1 TypeScript Compiler
- **核心特性**：类型检查、代码编译、配置选项
- **使用示例**：
  ```json
  {
    "compilerOptions": {
      "target": "ES2020",
      "module": "ESNext",
      "strict": true,
      "esModuleInterop": true,
      "skipLibCheck": true,
      "forceConsistentCasingInFileNames": true,
      "outDir": "./dist",
      "rootDir": "./src"
    },
    "include": ["src"]
  }
  ```

#### 4.3.2 ESLint
- **核心特性**：代码质量检查、风格规则、自定义规则
- **使用示例**：
  ```json
  {
    "extends": [
      "eslint:recommended",
      "plugin:@typescript-eslint/recommended",
      "plugin:react/recommended"
    ],
    "parser": "@typescript-eslint/parser",
    "plugins": ["@typescript-eslint", "react"],
    "rules": {
      "@typescript-eslint/explicit-function-return-type": "off",
      "react/prop-types": "off"
    }
  }
  ```

#### 4.3.3 Prettier
- **核心特性**：代码格式化、一致的代码风格
- **使用示例**：
  ```json
  {
    "semi": true,
    "trailingComma": "es5",
    "singleQuote": true,
    "printWidth": 80,
    "tabWidth": 2
  }
  ```

## 5. 最佳实践

### 5.1 代码组织

#### 5.1.1 目录结构
```
backend/src/
├── modules/        # 业务模块
│   ├── auth/       # 认证模块
│   ├── data/       # 数据模块
│   └── automation/ # 自动化模块
├── common/         # 公共组件
│   ├── guards/     # 守卫
│   ├── pipes/      # 管道
│   └── utils/      # 工具函数
├── main.ts         # 应用入口
└── app.module.ts   # 根模块

frontend/src/
├── components/     # 通用组件
├── pages/          # 页面组件
├── services/       # API 服务
├── store/          # Redux 状态管理
├── utils/          # 工具函数
├── main.tsx        # 应用入口
└── App.tsx         # 根组件
```

### 5.2 命名规范

#### 5.2.1 变量和函数
- **变量**：camelCase（例如：`userName`, `isActive`）
- **常量**：UPPER_SNAKE_CASE（例如：`MAX_RETRY_COUNT`, `API_URL`）
- **函数**：camelCase（例如：`getUser`, `calculateTotal`）
- **类**：PascalCase（例如：`UserService`, `AuthController`）
- **接口**：PascalCase，使用 I 前缀（例如：`IUser`, `ILoginRequest`）
- **类型**：PascalCase（例如：`UserType`, `Result`）

#### 5.2.2 文件和目录
- **文件**：camelCase 或 PascalCase（例如：`userService.ts`, `UserCard.tsx`）
- **目录**：kebab-case（例如：`user-profile`, `api-services`）

### 5.3 性能优化

#### 5.3.1 前端优化
- **代码分割**：使用 React.lazy 和 Suspense
- **虚拟列表**：处理大量数据列表
- **缓存**：使用 React Query 缓存 API 请求
- **减少重渲染**：使用 useMemo 和 useCallback
- **图片优化**：使用 WebP 格式和懒加载

#### 5.3.2 后端优化
- **数据库索引**：为频繁查询的字段创建索引
- **连接池**：使用数据库连接池
- **缓存**：使用 Redis 缓存热点数据
- **异步处理**：使用 async/await 和 Promise
- **批处理**：批量处理数据库操作

### 5.4 测试策略

#### 5.4.1 单元测试
```typescript
import { describe, it, expect } from 'vitest';
import { add } from './math';

describe('math functions', () => {
  it('should add two numbers correctly', () => {
    expect(add(1, 2)).toBe(3);
  });

  it('should handle negative numbers', () => {
    expect(add(-1, -2)).toBe(-3);
  });
});
```

#### 5.4.2 集成测试
```typescript
import { Test, TestingModule } from '@nestjs/testing';
import { AuthController } from './auth.controller';
import { AuthService } from './auth.service';

describe('AuthController', () => {
  let controller: AuthController;
  let service: AuthService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [AuthController],
      providers: [
        {
          provide: AuthService,
          useValue: {
            login: jest.fn().mockResolvedValue({ token: 'test-token' }),
          },
        },
      ],
    }).compile();

    controller = module.get<AuthController>(AuthController);
    service = module.get<AuthService>(AuthService);
  });

  it('should call login service', async () => {
    const result = await controller.login({ username: 'test', password: 'test' });
    expect(service.login).toHaveBeenCalled();
    expect(result).toEqual({ token: 'test-token' });
  });
});
```

## 6. 常见问题与解决方案

### 6.1 TypeScript 类型问题

#### 6.1.1 类型断言
```typescript
// 问题：类型不匹配
const user = {};
user.name = 'John'; // 错误：属性 'name' 不存在于类型 '{}' 上

// 解决方案：类型断言
interface User { name: string; }
const user = {} as User;
user.name = 'John'; // OK

// 或使用类型守卫
function isUser(obj: any): obj is User {
  return typeof obj === 'object' && obj !== null && 'name' in obj;
}

if (isUser(user)) {
  user.name = 'John'; // OK
}
```

#### 6.1.2 泛型约束
```typescript
// 问题：泛型类型过于宽泛
function logLength<T>(value: T): void {
  console.log(value.length); // 错误：类型 'T' 上不存在属性 'length'
}

// 解决方案：泛型约束
interface Lengthwise {
  length: number;
}

function logLength<T extends Lengthwise>(value: T): void {
  console.log(value.length); // OK
}

logLength('hello'); // 5
logLength([1, 2, 3]); // 3
```

### 6.2 模块化问题

#### 6.2.1 循环依赖
```typescript
// 问题：模块 A 依赖模块 B，模块 B 依赖模块 A
// moduleA.ts
import { funcB } from './moduleB';
export function funcA() { return funcB(); }

// moduleB.ts
import { funcA } from './moduleA';
export function funcB() { return funcA(); }

// 解决方案：
// 1. 提取共享逻辑到新模块
// 2. 使用依赖注入
// 3. 重构模块结构
```

### 6.3 函数式编程问题

#### 6.3.1 处理副作用
```typescript
// 问题：需要处理副作用（如 API 调用）
function fetchData(url: string): Promise<any> {
  return fetch(url).then(res => res.json());
}

// 解决方案：使用 monad 或函数包装
import { Either, left, right } from 'fp-ts/Either';

async function safeFetch(url: string): Promise<Either<Error, any>> {
  try {
    const response = await fetch(url);
    const data = await response.json();
    return right(data);
  } catch (error) {
    return left(error instanceof Error ? error : new Error('Unknown error'));
  }
}

// 使用
const result = await safeFetch('https://api.example.com/data');
if (result._tag === 'Right') {
  console.log('Success:', result.right);
} else {
  console.error('Error:', result.left);
}
```

## 7. 学习资源

### 7.1 官方文档
- [TypeScript 官方文档](https://www.typescriptlang.org/docs/)
- [NestJS 官方文档](https://docs.nestjs.com/)
- [React 官方文档](https://reactjs.org/docs/)
- [PostgreSQL 官方文档](https://www.postgresql.org/docs/)

### 7.2 在线资源
- [TypeScript Playground](https://www.typescriptlang.org/play)
- [MDN Web Docs](https://developer.mozilla.org/en-US/)
- [Stack Overflow](https://stackoverflow.com/)
- [GitHub](https://github.com/)

### 7.3 推荐书籍
- 《TypeScript 实战》
- 《深入理解 TypeScript》
- 《React 设计模式与最佳实践》
- 《Node.js 实战》

### 7.4 视频教程
- TypeScript 基础到进阶
- React + TypeScript 实战
- NestJS 从零到生产
- 函数式编程入门

---

## 8. 总结

TypeScript 是一种强大的类型化 JavaScript 超集，为大型应用开发提供了更好的工具和保障。本手册涵盖了 TypeScript 的核心语法、模块化开发、函数式开发以及项目中使用的技术栈，旨在为开发团队提供一份全面的参考资料。

通过遵循本手册中的最佳实践，结合模块化和函数式开发范式，可以构建出更加可维护、可测试和可扩展的应用。同时，不断学习和探索 TypeScript 及相关技术的新特性，将有助于提升开发效率和代码质量。

希望本手册能够成为您开发过程中的得力助手，随时为您提供必要的参考和指导。
