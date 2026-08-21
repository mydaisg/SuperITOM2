// 研发中心采访专栏 - 视频清单配置
// 新增一期采访：在 INTERVIEW_LIST 数组末尾追加一个对象即可
// 字段说明：
//   id       : 唯一标识（勿重复）
//   title    : 采访标题
//   video    : 视频文件相对路径（相对本文件所在 www 目录）
//   cover    : 封面图相对路径（可选，留空则使用默认占位封面）
//   dept     : 所属部门
//   issue    : 期号
//   date     : 发布日期
//   desc     : 简介
//   duration : 时长文本（可选，用于列表展示）

var INTERVIEW_LIST = [
  {
    id: "ai-2026-01",
    title: "研发中心采访 · AI应用开发部（2026年第1期专访）",
    video: "videos/interview_ai_2026_ep1.mp4",
    cover: "",
    dept: "AI应用开发部",
    issue: "2026年 第1期",
    date: "2026-08-21",
    desc: "走进 AI 应用开发部，聆听一线研发人员的真实声音，了解 AI 技术如何落地到实际业务场景。",
    duration: ""
  }
];
