const CACHE_NAME = 'shiny-auto-cache-v1';

// 安装时不预缓存，采用运行时缓存策略
self.addEventListener('install', event => {
  self.skipWaiting();
});

// 激活时清除旧缓存（可选）
self.addEventListener('activate', event => {
  event.waitUntil(
    caches.keys().then(keys => 
      Promise.all(keys.map(key => {
        if (key !== CACHE_NAME) return caches.delete(key);
      }))
    )
  );
  self.clients.claim();
});

// 拦截 fetch 请求并缓存静态资源
self.addEventListener('fetch', event => {
  const req = event.request;

  // 只缓存 GET 请求
  if (req.method !== 'GET') return;

  // 跳过某些动态请求（可根据需要调整）
  const url = new URL(req.url);
  if (url.pathname.startsWith('/api/') || url.pathname.includes('sockjs')) return;

  // 执行缓存逻辑：先查缓存，否则网络获取并写入缓存
  event.respondWith(
    caches.match(req).then(cached => {
      if (cached) return cached;

      return fetch(req).then(res => {
        // 排除某些无效响应（如 opaque 请求）
        if (!res || res.status !== 200 || res.type === 'opaque') {
          return res;
        }

        return caches.open(CACHE_NAME).then(cache => {
          cache.put(req, res.clone());
          return res;
        });
      }).catch(() => {
        // 网络失败时，回退机制（可选）
        return caches.match('/offline.html');
      });
    })
  );
});
