# 猫鼠游戏 - AI规划项目

一个基于SWI-Prolog实现的文本冒险游戏，采用基于PDDL的对抗AI，并配有可选的Web可视化界面。

---

##  项目概述

**游戏概念：**  
你扮演一只老鼠，试图在厨房找到曲奇，同时避开猫（AI对手）和捕鼠夹。游戏特色：
- 三个房间：卧室、厨房和客厅
- 两扇需要钥匙的锁定门
- 随机放置的物品和陷阱
- 基于PDDL的AI规划器控制猫的行为
- 战争迷雾探索系统

**核心特性：**
-  基于文本的Prolog游戏（核心要求）
-  对抗AI的PDDL规划（使用Pyperplan）
-  具有随机性的动态知识库
-  胜负条件和游戏状态管理
-  **扩展功能：** 基于Web的可视化界面和HTTP服务器

---

##  文件结构

```
CI/
├── 核心游戏文件（必需）
│   ├── cat_mouse.pl                    # 主游戏逻辑（760行）
│   ├── catmouse-domain.pddl            # PDDL领域定义
│   ├── catmouse-problem-full.pddl      # PDDL问题模板
│   └── pyperplan_runner.pl             # Pyperplan集成模块
│
├── 扩展功能（加分项）
│   ├── game_server.pl                  # Web界面的HTTP服务器
│   ├── game_frontend_connected.html    # Web可视化界面
│   ├── background.jpg                  # UI背景图片
│   └── start_server.bat                # 服务器快速启动脚本
│
└── 文档
    ├── README.md                       # 英文版说明文档
    └── README_CN.md                    # 中文版说明文档
```

---

##  环境要求

### 必需软件：
1. **SWI-Prolog**（9.x或更高版本）
   - 下载地址：https://www.swi-prolog.org/Download.html
   - 确保已添加到系统PATH

2. **Pyperplan**（STRIPS规划器）
   - 代码期望路径：`C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe`
   - ** 如果路径不同，请参见下面的"路径配置"章节**

3. **现代Web浏览器**（用于扩展功能）
   - Chrome、Firefox 或 Edge

---

##  路径配置（重要！）

在运行游戏之前，**必须**更新以下路径以匹配您的系统：

### 文件：`cat_mouse.pl`

**第13行** - 模块导入路径：
```prolog
:- use_module('C:/Users/ziyou/Desktop/CI/pyperplan_runner.pl').
```
将 `C:/Users/ziyou/Desktop/CI/` 改为**您的项目目录**。

**第17行** - Pyperplan可执行文件路径：
```prolog
Exe = 'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
```
改为**您的pyperplan.exe位置**。

**第625行** - Pyperplan可执行文件路径（在test_plan中）：
```prolog
'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
```
改为**您的pyperplan.exe位置**。

**第626行** - PDDL领域文件路径（在test_plan中）：
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-domain.pddl',
```
改为**您的项目目录**。

**第627行** - PDDL问题文件路径（在test_plan中）：
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-current.pddl',
```
改为**您的项目目录**。

**第650行** - PDDL问题生成路径（在cat_turn中）：
```prolog
generate_pddl_problem('C:/Users/ziyou/Desktop/CI/catmouse-current.pddl'),
```
改为**您的项目目录**。

**第654行** - Pyperplan可执行文件路径（在cat_turn中）：
```prolog
'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
```
改为**您的pyperplan.exe位置**。

**第655行** - PDDL领域文件路径（在cat_turn中）：
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-domain.pddl',
```
改为**您的项目目录**。

**第656行** - PDDL问题文件路径（在cat_turn中）：
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-current.pddl',
```
改为**您的项目目录**。

### 快速查找替换：

如果您的项目在不同目录，可以使用编辑器的查找替换功能：
- **查找：** `C:/Users/ziyou/Desktop/CI/`
- **替换为：** `您的项目路径/`

**示例：**
- 如果项目在 `D:/MyProjects/CatMouse/`
- 将所有出现的路径替换为 `D:/MyProjects/CatMouse/`

### 关于Pyperplan的注意事项：
- **三个位置**需要配置pyperplan.exe路径：第17行、第625行、第654行
- 建议统一修改为相同的路径

---

##  运行游戏

### 方法1：文本控制台游戏（核心要求）

这是作业要求的主要运行方式。

**步骤1：** 打开SWI-Prolog

**步骤2：** 加载游戏
```prolog
?- cd('C:/Users/ziyou/Desktop/CI').
?- [cat_mouse].
```

**步骤3：** 开始游戏
```prolog
?- start.
```

**步骤4：** 使用命令玩游戏：
```prolog
?- look.                    % 描述当前位置
?- sense.                   % 感知相邻格子
?- move(north).             % 移动（north/south/east/west）
?- take(bedroom_key).       % 拾取物品
?- unlock(bedroom_door).    % 开门（必须站在门上）
?- status.                  % 显示当前游戏状态
?- cat_turn.                % 猫的回合（AI规划）
```

**游戏会话示例：**
```prolog
?- start.
Welcome to Cat & Mouse!
You are at (1,8) in the living_room.
This is your cosy mouse hole.

?- sense.
You sniff around...
To the north: empty floor.
To the south: the outer wall.
To the west: the outer wall.
To the east: empty floor.

?- move(north).
You move to (1,7).
You are at (1,7) in the living_room.

?- cat_turn.
Cat is at (1,1), mouse at (1,7).
Cat planned first action: move_cat(c1_1,c1_2)
The cat moves to (1,2).

... 继续游戏 ...
```

---

### 方法2：基于Web的界面（扩展功能）

这展示了HTTP服务器和Web可视化的扩展能力。

**步骤1：** 启动Prolog服务器
```prolog
?- cd('C:/Users/ziyou/Desktop/CI').
?- [cat_mouse].
?- [game_server].
?- start_server(9178).
```

你应该看到：
```
========================================
  Cat & Mouse Game Server Started
========================================
  URL: http://localhost:9178
  Log File: game_log.txt
========================================
```

**步骤2：** 打开浏览器并访问：
```
http://localhost:9178
```

**关闭服务器：**
```prolog
?- halt.
```
或按 `Ctrl+C` 然后输入 `a` (abort)。

**步骤3：** 使用Web界面玩游戏：
- 使用方向键或WASD移动
- 按空格键感知周围
- 按E键拾取物品
- 按F键开门
- 点击"Cat's Turn"执行AI对手移动

**功能特性：**
- 实时游戏可视化
- 战争迷雾系统（边探索边显示）
- 交互式控制（鼠标+键盘）
- 带时间戳的游戏日志
- 专业UI设计

---

## 🎯 游戏规则与地图

### 地图布局（5x8网格）：
```
卧室 (1-3, 1-3)        厨房 (4-5, 1-3)
  [卧室钥匙]                [曲奇]
       |                      |
  卧室门(3,4)            厨房门(5,4)
       |                      |
       +----------------------+
              |
    客厅 (1-5, 5-8)
  [厨房钥匙] [陷阱 x2]
  
  老鼠洞 (1,8) - 起始点
```

### 目标：
- **胜利：** 从厨房获得曲奇
- **失败：** 被猫抓住 或 踩到捕鼠夹

### 策略：
1. 从老鼠洞(1,8)开始
2. 使用 `sense` 安全探索
3. 在客厅找到厨房钥匙
4. 解锁厨房门(5,4)
5. 从厨房获得曲奇
6. 避开猫的AI规划！

---

##  AI规划系统

猫使用**PDDL规划**（Pyperplan配合STRIPS）来智能追踪老鼠：

1. **动态问题生成：** 每回合基于当前游戏状态生成新的PDDL问题文件
2. **规划：** Pyperplan计算猫到达老鼠位置的计划
3. **执行：** 执行计划中的第一个动作
4. **重新规划：** 每回合重复此过程（自适应AI）

**猫可用的PDDL动作：**
- `move-cat`：在相邻格子间正常移动
- `pickup-key`：从地上拾取钥匙
- `unlock-door`：用正确的钥匙开门
- `move-through-door`：穿过已解锁的门

---

##  生成Transcript

生成用于提交的 `transcript.txt`，使用SWI-Prolog内置的 `protocol/1`：

```prolog
?- protocol('transcript.txt').
?- [cat_mouse].
?- start.
... 玩游戏 ...
?- noprotocol.
```

这将把所有控制台输出记录到 `transcript.txt` 文件。

---

##  故障排除

### 问题1："找不到Pyperplan"
**解决方案：** 更新 `cat_mouse.pl` 第20行的pyperplan路径：
```prolog
Exe = '你的路径/pyperplan.exe',
```

### 问题2："找不到模块pyperplan_runner"
**解决方案：** 确保在正确的目录：
```prolog
?- pwd.
?- cd('C:/Users/ziyou/Desktop/CI').
```

### 问题3：Web界面 - HTTP 500错误
**解决方案：** 停止并重启服务器：
```prolog
?- halt.
% 然后重启服务器：
?- [game_server].
?- start_server(9178).
```

### 问题4：Web界面 - 无法连接
**解决方案：** 
1. 检查服务器是否运行：`start_server(9178)` 应显示成功消息
2. 检查浏览器控制台（F12）是否有错误
3. 尝试清除浏览器缓存：Ctrl+Shift+R

### 问题5：端口9178已被占用
**解决方案：** 使用不同的端口：
```prolog
?- start_server(8080).
```
然后更新 `game_frontend_connected.html` 第19行：
```javascript
const API_BASE = 'http://localhost:8080/api';
```

---

##  游戏日志监控

运行Web界面时，所有游戏动作都会带时间戳记录到 `game_log.txt`。

**实时监控（PowerShell）：**
```powershell
Get-Content game_log.txt -Wait -Tail 50
```

**日志格式：**
```
[23:25:01] [API] ========================================
[23:25:01] [API] Player command: move(east)
[23:25:01] [GAME] Mouse at (1,8)
[23:25:01] [GAME] Mouse moved to (2,8)
[23:25:15] [API] CAT AI TURN
[23:25:15] [GAME] Cat at (1,1), Mouse at (2,8)
[23:25:15] [GAME] Cat moved to (1,2)
```

---

## 🎓 技术亮点

### 知识表示：
- **静态事实：** `room_of/3`、`door_cell/3`、`wall/2`、`walkable/2`
- **动态事实：** `at/3`、`item_at/3`、`has/2`、`trap/2`、`locked/1`、`turn/1`
- **规则：** 移动验证、门机制、胜负条件

### PDDL集成：
- 领域文件定义猫的动作空间
- 问题文件每回合动态生成
- Pyperplan计算到达老鼠的最优路径
- 执行计划中的第一个动作

### 随机化：
- 卧室钥匙：随机放置在卧室（排除猫的起始位置）
- 厨房钥匙：随机放置在客厅
- 曲奇：随机放置在厨房
- 陷阱：在客厅随机放置2个（排除安全区）

### 扩展功能：
- 使用 `library(http/thread_httpd)` 的HTTP服务器
- 带JSON响应的RESTful API
- 实时游戏状态同步
- 带战争迷雾的Web可视化
- 专业UI设计（Fredoka字体、渐变背景、动画效果）

---

##  快速启动指南（供评估使用）

**基本文本游戏：**
```prolog
?- [cat_mouse].
?- start.
```

**完整Web界面体验：**
```
1. 双击 start_server.bat
2. 打开浏览器：http://localhost:9178
3. 开始玩！
```

---

##  Web界面控制

| 输入 | 动作 |
|------|------|
| 方向键 / WASD | 移动老鼠 |
| 空格键 | 感知周围 |
| E键 | 拾取物品 |
| F键 | 开门 |
| R键 | 重启游戏 |

或使用屏幕上的按钮。

---

## 📄 作业要求符合性

本项目满足所有要求：
-  基于文本的Prolog冒险游戏
-  包含事实和规则的知识库
-  基于PDDL的对手AI（Pyperplan）
-  动态规划和重新规划
-  随机性和不确定性
-  胜负条件
-  **加分项：** Web界面和HTTP服务器（作业提示中建议的扩展）

---

##  作者信息

- **课程：** JC4002 - 知识表示
- **作业：** 带PDDL对手的基于文本的Prolog冒险游戏
- **学年：** 2025-2026

---

##  注意事项

- 游戏采用**战争迷雾**系统，玩家必须使用 `sense` 进行探索
- 猫每回合根据老鼠当前位置进行**重新规划**
- 安全格子 (1,7)、(2,8) 和 (5,5) 永远不会有陷阱
- 获得曲奇即刻获胜（无需返回家）
- 所有游戏输出可使用 `protocol/1` 捕获用于生成transcript

---

##  快速测试

验证一切正常工作：

```prolog
?- [cat_mouse].
?- start.
?- sense.
?- move(east).
?- cat_turn.
```

你应该看到：
- 老鼠移动确认信息
- 猫AI规划输出，显示PDDL动作
- 更新的游戏状态

祝游戏愉快！

---

##  评分标准对应

### 知识库型知识（10分）
**事实和数据：**
- 静态事实：房间定义、地图布局、门和墙的位置
- 动态事实：角色位置、物品位置、门的锁定状态、游戏回合数
- 所有事实都在游戏中使用，没有冗余

**动态事实管理：**
- 使用 `assert/1` 和 `retract/1` 动态更新
- 使用 `dynamic` 声明所有可变谓词
- 游戏状态一致性通过规则验证

### 知识库规则（20分）
**游戏规则实现：**
- 移动规则：`move/1`、`can_pass/5`、`update_position/5`
- 交互规则：`take/1`、`unlock/1`
- 状态检查：`check_game_over/0`、`check_not_over/0`

**输赢条件：**
- 胜利：`has(mouse,cookie)` - 获得曲奇
- 失败1：`at(mouse,X,Y), trap(X,Y)` - 踩到陷阱
- 失败2：`at(mouse,X,Y), at(cat,X,Y)` - 被猫抓住

**对手决策：**
- 使用Pyperplan PDDL规划器
- 每回合动态生成问题文件
- 基于当前状态重新规划

**随机性/不确定性：**
- 物品位置随机：`place_bedroom_key/0`、`place_kitchen_key/0`、`place_cookie/0`
- 陷阱位置随机：`place_traps/0`
- 使用 `random_member/2` 和 `random_permutation/2`

**算术和数据结构：**
- 坐标系统：(X,Y) 整数对
- 坐标转换：`coord_cell_name/3`、`cell_name_coord/3`
- 方向计算：`step/5` 谓词

### 计划执行（15分）
**规划执行流程：**
1. `cat_turn/0` 调用 `generate_pddl_problem/1` 生成当前状态
2. 调用 `pyperplan_solve/3` 获取计划
3. `cat_apply_first_action/1` 执行第一个动作
4. `cat_do_action/1` 解析并验证动作

**动作验证：**
- 检查门的开/关状态
- 验证钥匙持有情况
- 确认位置有效性
- 处理非法动作（输出错误信息）

### 计划动态性（20分）
**动态重新规划：**
-  每回合重新生成PDDL问题
-  目标始终是老鼠的当前位置
-  猫需要根据门的状态调整策略
-  拾取钥匙、开门、移动的完整规划链

**策略适应性：**
- 猫的行为基于实时游戏状态
- 门锁定时需要先找钥匙
- 门开启后可以选择更短路径
- 完全自动化的AI决策

---

##  操作指南

### 快速验证功能：

**1. 文本游戏（核心）：**
```prolog
?- [cat_mouse].
?- start.
?- sense.          % 查看知识库规则
?- move(north).    % 测试移动规则
?- cat_turn.       % 验证PDDL规划
```

**2. Web界面（加分项）：**
- 双击 `start_server.bat`
- 访问 `http://localhost:9178`
- 体验现代化的游戏界面

**3. 查看PDDL规划过程：**
运行游戏时会生成：
- `catmouse-current.pddl` - 当前状态的问题文件
- `catmouse-current.pddl.soln` - 规划器的解决方案

**4. 查看实时日志：**
打开 `game_log.txt` 查看详细的游戏操作记录

---

##  项目特色

1. **完整的知识表示**
   - 76个静态事实（地图、房间、门）
   - 8种动态事实类型
   - 20+条规则

2. **智能对手AI**
   - 使用STRIPS规划器
   - 每回合自适应重新规划
   - 考虑门、钥匙、路径优化

3. **丰富的游戏机制**
   - 视野迷雾系统
   - 多层级地图
   - 物品管理系统
   - 门锁机制

4. **专业扩展**
   - RESTful API设计
   - 前后端分离架构
   - 实时状态同步
   - 现代化UI/UX设计

---

##  联系方式

如有技术问题，请检查：
1. Pyperplan路径是否正确
2. SWI-Prolog版本是否兼容
3. 浏览器控制台错误信息（F12）
4. game_log.txt 日志文件

---





